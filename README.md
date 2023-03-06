
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyr <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr)
[![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr?branch=main)
<!-- badges: end -->

## Overview

dplyr is a grammar of data manipulation, providing a consistent set of
verbs that help you solve the most common data manipulation challenges:

- `mutate()` adds new variables that are functions of existing variables
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

These all combine naturally with `group_by()` which allows you to
perform any operation “by group”. You can learn more about them in
`vignette("dplyr")`. As well as these single-table verbs, dplyr also
provides a variety of two-table verbs, which you can learn about in
`vignette("two-table")`.

If you are new to dplyr, the best place to start is the [data
transformation chapter](https://r4ds.had.co.nz/transform.html) in R for
data science.

## Backends

In addition to data frames/tibbles, dplyr makes working with other
computational backends accessible and efficient. Below is a list of
alternative backends:

- [arrow](https://arrow.apache.org/docs/r/) for larger-than-memory
  datasets, including on remote cloud storage like AWS S3, using the
  Apache Arrow C++ engine,
  [Acero](https://arrow.apache.org/docs/cpp/streaming_execution.html).

- [dtplyr](https://dtplyr.tidyverse.org/) for large, in-memory datasets.
  Translates your dplyr code to high performance
  [data.table](https://rdatatable.gitlab.io/data.table/) code.

- [dbplyr](https://dbplyr.tidyverse.org/) for data stored in a
  relational database. Translates your dplyr code to SQL.

- [duckdb](https://duckdb.org/docs/api/r) for large datasets that are
  still small enough to fit on your computer.

- [sparklyr](https://spark.rstudio.com) for very large datasets stored
  in [Apache Spark](https://spark.apache.org).

## Installation

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
install.packages("dplyr")
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of dplyr from GitHub.

``` r
# install.packages("pak")
pak::pak("tidyverse/dplyr")
```

## Cheat Sheet

<a href="https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/main/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png" width="630" height="252"/></a>

## Usage

``` r
library(dplyr)

starwars %>% 
  filter(species == "Droid")
#> # A tibble: 6 × 14
#>   name   height  mass hair_color skin_color eye_c…¹ birth…² sex   gender homew…³
#>   <chr>   <int> <dbl> <chr>      <chr>      <chr>     <dbl> <chr> <chr>  <chr>  
#> 1 C-3PO     167    75 <NA>       gold       yellow      112 none  mascu… Tatooi…
#> 2 R2-D2      96    32 <NA>       white, bl… red          33 none  mascu… Naboo  
#> 3 R5-D4      97    32 <NA>       white, red red          NA none  mascu… Tatooi…
#> 4 IG-88     200   140 none       metal      red          15 none  mascu… <NA>   
#> 5 R4-P17     96    NA none       silver, r… red, b…      NA none  femin… <NA>   
#> # … with 1 more row, 4 more variables: species <chr>, films <list>,
#> #   vehicles <list>, starships <list>, and abbreviated variable names
#> #   ¹​eye_color, ²​birth_year, ³​homeworld

starwars %>% 
  select(name, ends_with("color"))
#> # A tibble: 87 × 4
#>   name           hair_color skin_color  eye_color
#>   <chr>          <chr>      <chr>       <chr>    
#> 1 Luke Skywalker blond      fair        blue     
#> 2 C-3PO          <NA>       gold        yellow   
#> 3 R2-D2          <NA>       white, blue red      
#> 4 Darth Vader    none       white       yellow   
#> 5 Leia Organa    brown      light       brown    
#> # … with 82 more rows

starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)
#> # A tibble: 87 × 4
#>   name           height  mass   bmi
#>   <chr>           <int> <dbl> <dbl>
#> 1 Luke Skywalker    172    77  26.0
#> 2 C-3PO             167    75  26.9
#> 3 R2-D2              96    32  34.7
#> 4 Darth Vader       202   136  33.3
#> 5 Leia Organa       150    49  21.8
#> # … with 82 more rows

starwars %>% 
  arrange(desc(mass))
#> # A tibble: 87 × 14
#>   name         height  mass hair_…¹ skin_…² eye_c…³ birth…⁴ sex   gender homew…⁵
#>   <chr>         <int> <dbl> <chr>   <chr>   <chr>     <dbl> <chr> <chr>  <chr>  
#> 1 Jabba Desil…    175  1358 <NA>    green-… orange    600   herm… mascu… Nal Hu…
#> 2 Grievous        216   159 none    brown,… green,…    NA   male  mascu… Kalee  
#> 3 IG-88           200   140 none    metal   red        15   none  mascu… <NA>   
#> 4 Darth Vader     202   136 none    white   yellow     41.9 male  mascu… Tatooi…
#> 5 Tarfful         234   136 brown   brown   blue       NA   male  mascu… Kashyy…
#> # … with 82 more rows, 4 more variables: species <chr>, films <list>,
#> #   vehicles <list>, starships <list>, and abbreviated variable names
#> #   ¹​hair_color, ²​skin_color, ³​eye_color, ⁴​birth_year, ⁵​homeworld

starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    mass > 50
  )
#> # A tibble: 8 × 3
#>   species      n  mass
#>   <chr>    <int> <dbl>
#> 1 Droid        6  69.8
#> 2 Gungan       3  74  
#> 3 Human       35  82.8
#> 4 Kaminoan     2  88  
#> 5 Mirialan     2  53.1
#> # … with 3 more rows
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/tidyverse/dplyr/issues). For questions and
other discussion, please use
[community.rstudio.com](https://community.rstudio.com/) or the
[manipulatr mailing list](https://groups.google.com/d/forum/manipulatr).

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT). By participating
in this project you agree to abide by its terms.
