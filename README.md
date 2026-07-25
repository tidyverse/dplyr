<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyr <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" alt="dplyr package hexagon logo" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr)
[![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dplyr/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Contents

- [Overview](#overview)
- [Learning more](#learning-more)
- [Backends](#backends)
- [Installation](#installation)
- [Cheat Sheet](#cheat-sheet)
- [Usage](#usage)
- [Getting help](#getting-help)
- [FAQ](#faq-dplyrs-filterlag-conflict-with-base-r)
- [Citing dplyr](#citing-dplyr)

## Overview

dplyr is a grammar of data manipulation, providing a consistent set of
verbs that help you solve the most common data manipulation challenges:

- `mutate()` adds new variables that are functions of existing variables.
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

These all combine naturally with `group_by()` which allows you to
perform any operation “by group”. You can learn more about them in
`vignette("dplyr")`. As well as these single-table verbs, dplyr also
provides a variety of two-table verbs, which you can learn about in
`vignette("two-table")`. See `?dplyr::join` for a full list of joining
verbs, including `inner_join()`, `left_join()`, and `anti_join()`.

If you are new to dplyr, the best place to start is the [data
transformation chapter](https://r4ds.hadley.nz/data-transform) in R for
Data Science.

## Learning more

- [Data transformation chapter](https://r4ds.hadley.nz/data-transform) in *R for Data Science* — the best starting point for beginners.
- `vignette("dplyr")` — an introduction to the grammar, run from R.
- `vignette("two-table")` — joins and other multi-table verbs.
- [Cheat Sheet](#cheat-sheet) — a one-page visual reference (see below).
- [Function reference](https://dplyr.tidyverse.org/reference/index.html) — a full list of all exported functions.

## Backends

In addition to data frames/tibbles, dplyr makes working with other
computational backends accessible and efficient. Below is a list of
alternative backends:

- [arrow](https://arrow.apache.org/docs/r/) for larger-than-memory
  datasets, including on remote cloud storage like AWS S3, using the
  Apache Arrow C++ engine,
  [Acero](https://arrow.apache.org/docs/cpp/acero/overview.html).

- [dbplyr](https://dbplyr.tidyverse.org/) for data stored in a
  relational database. Translates your dplyr code to SQL.

- [dtplyr](https://dtplyr.tidyverse.org/) for large, in-memory datasets.
  Translates your dplyr code to high-performance
  [data.table](https://rdatatable.gitlab.io/data.table/) code.

- [duckplyr](https://duckplyr.tidyverse.org/) for large, in-memory
  datasets. Translates your dplyr code to high-performance
  [duckdb](https://duckdb.org) queries with zero extra copies and an
  automatic R fallback when translation isn’t possible.

- [sparklyr](https://spark.posit.co/) for very large datasets stored in
  [Apache Spark](https://spark.apache.org).

| Package | Best for |
|---|---|
| [arrow](https://arrow.apache.org/docs/r/) | Larger-than-memory / cloud-stored data |
| [dbplyr](https://dbplyr.tidyverse.org/) | Data in a relational database (translates to SQL) |
| [dtplyr](https://dtplyr.tidyverse.org/) | Large in-memory data (translates to data.table) |
| [duckplyr](https://duckplyr.tidyverse.org/) | Large in-memory data via DuckDB, zero-copy |
| [sparklyr](https://spark.posit.co/) | Very large data on Apache Spark |

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

<a href="https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/main/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png" width="630" height="252" alt="dplyr data transformation cheat sheet thumbnail"/></a>

## Usage

*(Note: output below is truncated by tibble's default print method — `#> # ℹ N more rows` means there are more rows than what's shown, not that the operation failed.)*

``` r
library(dplyr)

# starwars is a sample dataset that ships with dplyr — no separate install needed
starwars |>
  filter(species == "Droid")
#> # A tibble: 6 × 14
#>   name   height  mass hair_color skin_color  eye_color birth_year sex   gender  
#>   <chr>   <int> <dbl> <chr>      <chr>       <chr>          <dbl> <chr> <chr>   
#> 1 C-3PO     167    75 <NA>       gold        yellow           112 none  masculi…
#> 2 R2-D2      96    32 <NA>       white, blue red               33 none  masculi…
#> 3 R5-D4      97    32 <NA>       white, red  red               NA none  masculi…
#> 4 IG-88     200   140 none       metal       red               15 none  masculi…
#> 5 R4-P17     96    NA none       silver, red red, blue         NA none  feminine
#> # ℹ 1 more row
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

starwars |>
  select(name, ends_with("color"))
#> # A tibble: 87 × 4
#>   name           hair_color skin_color  eye_color
#>   <chr>          <chr>      <chr>       <chr>    
#> 1 Luke Skywalker blond      fair        blue     
#> 2 C-3PO          <NA>       gold        yellow   
#> 3 R2-D2          <NA>       white, blue red      
#> 4 Darth Vader    none       white       yellow   
#> 5 Leia Organa    brown      light       brown    
#> # ℹ 82 more rows

starwars |>
  rename(homeworld_planet = homeworld)

starwars |>
  mutate(name, bmi = mass / ((height / 100)^2)) |>
  select(name:mass, bmi)
#> # A tibble: 87 × 4
#>   name           height  mass   bmi
#>   <chr>           <int> <dbl> <dbl>
#> 1 Luke Skywalker    172    77  26.0
#> 2 C-3PO             167    75  26.9
#> 3 R2-D2              96    32  34.7
#> 4 Darth Vader       202   136  33.3
#> 5 Leia Organa       150    49  21.8
#> # ℹ 82 more rows

starwars |>
  arrange(desc(mass))
#> # A tibble: 87 × 14
#>   name      height  mass hair_color skin_color eye_color birth_year sex   gender
#>   <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#> 1 Jabba De…    175  1358 <NA>       green-tan… orange         600   herm… mascu…
#> 2 Grievous     216   159 none       brown, wh… green, y…       NA   male  mascu…
#> 3 IG-88        200   140 none       metal      red             15   none  mascu…
#> 4 Darth Va…    202   136 none       white      yellow          41.9 male  mascu…
#> 5 Tarfful      234   136 brown      brown      blue            NA   male  mascu…
#> # ℹ 82 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

# group_by() + summarise() collapses each group to one row;
# the filter() below then filters those summarised rows, not the original data
starwars |>
  group_by(species) |>
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) |>
  filter(
    n > 1,
    mass > 50
  )
#> # A tibble: 9 × 3
#>   species      n  mass
#>   <chr>    <int> <dbl>
#> 1 Droid        6  69.8
#> 2 Gungan       3  74  
#> 3 Human       35  81.3
#> 4 Kaminoan     2  88  
#> 5 Mirialan     2  53.1
#> # ℹ 4 more rows
```

## Getting help

- Browse the full [function reference](https://dplyr.tidyverse.org/reference/index.html) for a list of all exported functions.
- If you encounter a clear bug, please file an issue with a minimal reproducible example on
  [GitHub](https://github.com/tidyverse/dplyr/issues).
- For questions and other discussion, please use
  [forum.posit.co](https://forum.posit.co).

Please note that this project is released with a
[Contributor Code of Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.

## FAQ: dplyr's filter()/lag() conflict with base R

When you load dplyr, you may see:

```
The following objects are masked from 'package:stats':

    filter, lag
```

This is expected — dplyr's `filter()` and `lag()` intentionally override the
base `stats` versions. To call the base version explicitly, use
`stats::filter()` or `stats::lag()`.

## Citing dplyr

If you use dplyr in a publication, please cite it with:

``` r
citation("dplyr")
```
