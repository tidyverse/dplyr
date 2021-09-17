library(tidyverse)

# Creates storms data set from NOAA Atlantic Hurricane data, which is provided
# in an unorthodox format: a csv that alternates between header/identifier rows
# and data rows.

# TO UPDATE: get the latest URL from https://www.nhc.noaa.gov/data/#hurdat, and rerun this code

# Read in data set so each line is a character string
storm_file_complete <- read_file("https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2020-052921.txt")
storm_strings <- read_lines(storm_file_complete)

# Identify the header lines that have three commas
header_locations <- (1:length(storm_strings))[str_count(storm_strings, "\\,") == 3]

# Extract length of each sub-dataset
headers <- as.list(storm_strings[header_locations])
headers_df <- headers %>%
  map(str_sub, start = 1, end = -2) %>% # to remove trailing comma
  map(paste0, "\n") %>%                 # to trigger literal read
  map_df(read_csv, col_names = c("id", "name", "n_obs")) %>%
  mutate(name = recode(name, "UNNAMED" = id), skip = header_locations) %>%
  select(name, skip, n_obs)

column_types <- list(
  date = col_character(),
  time = col_character(),
  record_type = col_character(),
  status = col_character(),
  lat = col_character(),
  long = col_character(),
  wind = col_integer(),
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
  extent_64_NW = col_integer(),
  nas = col_integer()
)
column_names <- names(column_types)


#### Parse each storm as its own sub-dataframe
storm_dataframes <- vector("list", nrow(headers_df))
for (i in 1:nrow(headers_df)) {
  # get this storm's metadata
  row_start = headers_df[i,]$skip + 1
  row_end = headers_df[i,]$n_obs + row_start - 1
  # subset of rows belonging to this storm
  data_subset = storm_strings[row_start:row_end] %>%
    paste(collapse = "\n") %>%
    paste0("\n")
  data_subset = read_csv(
    data_subset,
    col_names = column_names,
    col_types = column_types,
    na = c("", "-99", "-999")
  )
  # name at the front
  data_subset$name = headers_df[i,]$name
  data_subset = data_subset %>% relocate(name)
  # add to list of storms
  storm_dataframes[[i]] = data_subset
}

# Combine and clean the data sets
library(lubridate)

storms <- storm_dataframes %>%
  bind_rows() %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date),
    day = day(date),
    hour = as.numeric(str_sub(time, 1, 2)),
    lat_hemisphere = str_sub(lat, -1),
    lat_sign = if_else(lat_hemisphere == "N", 1, -1),
    lat = as.numeric(str_sub(lat, 1, -2)) * lat_sign,
    long_hemisphere = str_sub(long, -1),
    long_sign = if_else(long_hemisphere == "E", 1, -1),
    long = as.numeric(str_sub(long, 1, -2)) * long_sign,
    category = cut(wind,
      breaks = c(0, 34, 64, 83, 96, 113, 137, 500),
      labels = c(-1, 0, 1, 2, 3, 4, 5),
      include.lowest = TRUE, ordered = TRUE
    ),
    # wind = wind * 1.15078, # transforms knots to mph,
    TSradius1 = extent_34_NE + extent_34_SW,
    TSradius2 = extent_34_NW + extent_34_SE,
    tropicalstorm_force_diameter = pmax(TSradius1, TSradius2),
    HUradius1 = extent_64_NE + extent_64_SW,
    HUradius2 = extent_64_NW + extent_64_SE,
    hurricane_force_diameter = pmax(HUradius1, HUradius2),
    status = recode(status, "HU" = "hurricane", "TS" = "tropical storm", "TD" = "tropical depression")
  ) %>%
  select(name, year, month, day, hour, lat, long, status, category, wind, pressure, tropicalstorm_force_diameter, hurricane_force_diameter)

# Narrow to storms that have complete pressure record
completeish <- storms %>%
  group_by(name) %>%
  summarise(n_pressure = sum(!is.na(pressure)), p_pressure = mean(!is.na(pressure))) %>%
  filter(p_pressure == 1) %>%
  .[["name"]]

storms <- storms %>%
  filter(
    status %in% c("hurricane", "tropical storm", "tropical depression"),
    name %in% completeish
  ) %>%
  mutate(name = if_else(str_sub(name, 1, 3) %in% c("AL0", "AL1"), name, str_to_title(name)))

# output for the package
usethis::use_data(storms, overwrite = TRUE)
