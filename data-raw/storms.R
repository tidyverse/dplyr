library(tidyverse)

# Creates storms data set from NOAA Atlantic Hurricane data, which is provided
# in an unorthodox format: a csv that alternates between header/identifier rows
# and data rows.

# Read in data set so each line is a character string
storm_strings <- read_lines("http://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2015-070616.txt")

# Identify the header lines that have three commas
library(stringr)
header_locations <- (1:length(storm_strings))[str_count(storm_strings, "\\,") == 3]

# Extract length of each sub-dataset
headers <- as.list(storm_strings[header_locations])
headers_df <- headers %>%
  map(str_sub, start = 1, end = -2) %>% # to remove trailing comma
  map(paste0, "\n") %>%                 # to trigger literal read
  map_df(read_csv, col_names = c("id", "name", "n_obs")) %>%
  mutate(name = recode(name, "UNNAMED" = id), skip = header_locations) %>%
  select(name, skip, n_obs)

# Read in the sub-datasets as data frames
df_names <- c("date", "time", "record_type", "status", "lat", "long", "wind", "pressure",
  "extent_34_NE",  "extent_34_SE",  "extent_34_SW",  "extent_34_NW",
  "extent_50_NE",  "extent_50_SE",  "extent_50_SW",  "extent_50_NW",
  "extent_64_NE",  "extent_64_SE",  "extent_64_SW",  "extent_64_NW", "nas")

storm_dfs <- vector("list", nrow(headers_df))
names(storm_dfs) <- headers_df$name

for (i in seq_along(headers_df$name)) {
  storm_dfs[[i]] <- read_csv("data-raw/hurdat2.txt",
    skip = headers_df$skip[i],
    n_max = headers_df$n_obs[i],
    col_names = df_names,
    na = c("", "-99", "-999"),
    col_types = list(
      time = col_character(),
      pressure = col_integer(),
      extent_34_NE = col_integer(),
      extent_34_SE = col_integer(),
      extent_34_SW = col_integer(),
      extent_34_NW = col_integer(),
      extent_50_NE = col_integer(),
      extent_50_SE = col_integer(),
      extent_50_SW = col_integer(),
      extent_50_NW = col_integer(),
      extent_64_NE = col_integer(),
      extent_64_SE = col_integer(),
      extent_64_SW = col_integer(),
      extent_64_NW = col_integer()
  ))
}

# Combine and clean the data sets
library(lubridate)

storms <- storm_dfs %>%
  bind_rows(.id = "name") %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date),
    day = day(date),
    hour = as.numeric(str_sub(time, 1, 2)),
    lat_hemisphere =  str_sub(lat, -1),
    lat_sign = if_else(lat_hemisphere == "N", 1, -1),
    lat = as.numeric(str_sub(lat, 1, -2)) * lat_sign,
    long_hemisphere =  str_sub(long, -1),
    long_sign = if_else(long_hemisphere == "E", 1, -1),
    long = as.numeric(str_sub(long, 1, -2)) * long_sign,
    category = cut(wind, breaks = c(0, 34, 64, 83, 96, 113, 137, 500),
    labels = c(-1, 0, 1, 2, 3, 4, 5),
    include.lowest = TRUE, ordered = TRUE),
    # wind = wind * 1.15078, # transforms knots to mph,
    TSradius1 = extent_34_NE + extent_34_SW,
    TSradius2 = extent_34_NW + extent_34_SE,
    ts_diameter = pmax(TSradius1, TSradius2) * 1.15078, # to convert from nautical miles to miles
    HUradius1 = extent_64_NE + extent_64_SW,
    HUradius2 = extent_64_NW + extent_64_SE,
    hu_diameter = pmax(HUradius1, HUradius2) * 1.15078, # to convert from nautical miles to miles
    status = recode(status, "HU" = "hurricane", "TS" = "tropical storm", "TD" = "tropical depression")) %>%
  select(name, year, month, day, hour, lat, long, status, category, wind, pressure, ts_diameter, hu_diameter)

# Narrow to storms that have complete pressure record
completeish <- storms %>%
  group_by(name) %>%
  summarise(n_pressure = sum(!is.na(pressure)), p_pressure = mean(!is.na(pressure))) %>%
  filter(p_pressure == 1) %>%
  .[["name"]]

storms <- storms %>%
  filter(
    status %in% c("hurricane", "tropical storm", "tropical depression"),
    name %in% completeish) %>%
  mutate(name = if_else(str_sub(name, 1, 3) %in% c("AL0", "AL1"), name, str_to_title(name)))

devtools::use_data(storms)
