library(tidyverse)
library(httr)

get_all <- function(url) {
  out <- NULL

  while (!is.null(url)) {
    message("Getting ", url)
    req <- GET(url)
    stop_for_status(req)

    con <- content(req)
    out <- c(out, con$results)
    url <- con$`next`
  }

  out
}

people <- get_all("http://swapi.co/api/people")
str(people[[1]])


# Generate lookup tables --------------------------------------------------

lookup <- function(url, name = "name") {
  all <- get_all(url)

  url <- all %>% map_chr("url")
  name <- all %>% map_chr(name)
  name[name == "unknown"] <- NA

  set_names(name, url)
}

species <- lookup("http://swapi.co/api/species")
films <- lookup("http://swapi.co/api/films", "title")
planets <- lookup("http://swapi.co/api/planets")
vehicles <- lookup("http://swapi.co/api/vehicles")
starships <- lookup("http://swapi.co/api/starships")

starwars <- tibble(
  name = people %>% map_chr("name"),
  height = people %>% map_chr("height") %>% parse_integer(na = "unknown"),
  mass = people %>% map_chr("mass") %>% parse_number(na = "unknown"),
  hair_color = people %>% map_chr("hair_color") %>% parse_character(na = "n/a"),
  skin_color = people %>% map_chr("skin_color"),
  eye_color = people %>% map_chr("eye_color"),
  birth_year = people %>% map_chr("birth_year") %>% parse_number(na = "unknown"),
  sex = people %>% map_chr("gender") %>% parse_character(na = "n/a"),
  gender = NA_character_,
  homeworld = people %>% map_chr("homeworld") %>% planets[.] %>% unname(),
  species = people %>% map("species") %>% map_chr(1, .null = NA) %>% species[.] %>% unname(),
  films = people %>% map("films") %>% map(. %>% flatten_chr() %>% films[.] %>% unname()),
  vehicles = people %>% map("vehicles", .default = list()) %>% map(. %>% flatten_chr() %>% vehicles[.] %>% unname()),
  starships = people %>% map("starships", .default = list()) %>% map(. %>% flatten_chr() %>% starships[.] %>% unname())
)


# Add gender --------------------------------------------------------------

# For asexual and hermaphroditic individuals we need to add some manually
# collected data from @MeganBeckett.

# Droids are robots and do not have a biological sex. But they
# they have a gender, determined by they were programmed:
# https://starwars.fandom.com/wiki/Sexes
#
# Hutts are hermaphroditic, but identify as either male or female

genders <- c(
  "C-3PO" = "masculine",  # https://starwars.fandom.com/wiki/C-3PO
  "BB8" = "masculine",    # https://starwars.fandom.com/wiki/BB-8
  "IG-88" = "masculine",  # https://starwars.fandom.com/wiki/IG-88
  "R5-D4" = "masculine",  # https://starwars.fandom.com/wiki/R5-D4
  "R2-D2" = "masculine",  # https://starwars.fandom.com/wiki/R2-D2
  "R4-P17" = "feminine",  # https://starwars.fandom.com/wiki/R4-P17
  "Jabba Desilijic Tiure" = "masculine" # https://starwars.fandom.com/wiki/Jabba_Desilijic_Tiure
)

starwars <- mutate(starwars,
  species = ifelse(name == "R4-P17", "droid", species), # R4-P17 is a droid
  sex = ifelse(species == "droid", "none", sex), # Droids don't have biological sex
  gender = case_when(
    sex == "male" ~ "masculine",
    sex == "female" ~ "feminine",
    TRUE ~ unname(genders[name])
  )
)

starwars %>% filter(is.na(sex)) %>% select(name, species, gender, sex)

starwars %>% count(gender, sort = TRUE)
starwars %>% count(sex, gender, sort = TRUE)

# Basic checks -------------------------------------------------------------

starwars %>% count(species, sort = TRUE)
starwars %>% count(homeworld, sort = TRUE)
starwars %>% count(skin_color, sort = TRUE)

starwars %>% group_by(species) %>% summarise(mass = mean(mass, na.rm = T))

# Save --------------------------------------------------------------------

# Save in convenient form for diffs
starwars %>%
  mutate_if(is.list, ~ map_chr(., paste, collapse = ", ")) %>%
  write_csv("data-raw/starwars.csv")

usethis::use_data(starwars, overwrite = TRUE)
