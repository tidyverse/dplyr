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
  homeworld = people %>% map_chr("homeworld") %>% planets[.] %>% unname(),
  species = people %>% map("species") %>% map_chr(1, .null = NA) %>% species[.] %>% unname(),
  films = people %>% map("films") %>% map(. %>% flatten_chr() %>% films[.] %>% unname()),
  vehicles = people %>% map(~.[["vehicles"]]) %>% map(. %>% flatten_chr() %>% vehicles[.] %>% unname()),
  starships = people %>% map(~.[["starships"]]) %>% map(. %>% flatten_chr() %>% starships[.] %>% unname())
) %>%
  mutate(sex = case_when(sex == 'hermaphrodite' ~ 'hermaphroditic',
                         sex == 'none' ~ 'asexual',
                         TRUE ~ sex))

# A little exploration
starwars %>% count(species, sort = TRUE)
starwars %>% count(homeworld, sort = TRUE)
starwars %>% count(skin_color, sort = TRUE)
starwars %>% count(sex, sort = TRUE)

starwars %>% group_by(species) %>% summarise(mass = mean(mass, na.rm = T))

devtools::use_data(starwars, overwrite = TRUE)
