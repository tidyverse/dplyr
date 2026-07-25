markdown
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyr <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr)
[![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dplyr/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr)
<!-- badges: end -->

## Project Status

The badges above provide quick insight into dplyr's current status:
- **CRAN status**: Shows the version available on CRAN and whether it passes checks
- **R-CMD-check**: Indicates that the package passes R's comprehensive package checks
- **Codecov test coverage**: Shows the percentage of code covered by tests

All badges should show passing/healthy status. If you see any failures, please [open an issue](https://github.com/tidyverse/dplyr/issues/new/choose).

## Overview

dplyr is a grammar of data manipulation, providing a consistent set of
verbs that help you solve the most common data manipulation challenges:

- `mutate()` adds new variables that are functions of existing variables
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

These all combine naturally with `group_by()` which allows you to
perform any operation "by group". You can learn more about them in
`vignette("dplyr")`. As well as these single-table verbs, dplyr also
provides a variety of two-table verbs, which you can learn about in
`vignette("two-table")`.

If you are new to dplyr, the best place to start is the [data
transformation chapter](https://r4ds.hadley.nz/data-transform) in R for
Data Science.

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
  Translates your dplyr code to high performance
  [data.table](https://rdatatable.gitlab.io/data.table/) code.

- [duckplyr](https://duckplyr.tidyverse.org/) for large, in-memory
  datasets. Translates your dplyr code to high performance
  [duckdb](https://duckdb.org) queries with zero extra copies and an
  automatic R fallback when translation isn't possible.

- [sparklyr](https://spark.posit.co/) for very large datasets stored in
  [Apache Spark](https://spark.apache.org).

## Installation

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
install.packages("dplyr")
Development version
To get a bug fix or to use a feature from the development version, you
can install the development version of dplyr from GitHub.

r
# install.packages("pak")
pak::pak("tidyverse/dplyr")

# Alternative using remotes:
# install.packages("remotes")
# remotes::install_github("tidyverse/dplyr")
Cheat Sheet
<a href="https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/main/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png" width="630" height="252"/></a>

Usage
r
library(dplyr)

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

# Conditional transformations with case_when()
starwars |>
  mutate(
    size_category = case_when(
      height < 100 ~ "Small",
      height < 170 ~ "Medium",
      height >= 170 ~ "Tall",
      TRUE ~ "Unknown"
    )
  ) |>
  select(name, height, size_category) |>
  head()

# Relocate columns to specific positions
starwars |>
  relocate(name, species, .before = height) |>
  select(name:mass) |>
  head()

# Case-insensitive filtering example
starwars |>
  filter(tolower(name) == "c-3po")

# Get distinct combinations of multiple columns
mtcars |>
  distinct(cyl, am) |>
  arrange(cyl, am)

# Get distinct rows based on certain columns
mtcars |>
  distinct(cyl, am, .keep_all = TRUE) |>
  head()

# Multiple summary functions with grouped data
mtcars |>
  group_by(cyl) |>
  summarise(
    count = n(),
    avg_mpg = mean(mpg),
    sd_mpg = sd(mpg),
    max_hp = max(hp),
    .groups = "drop"
  )

# Group by multiple variables
mtcars |>
  group_by(cyl, am) |>
  summarise(
    avg_mpg = mean(mpg),
    n = n(),
    .groups = "drop"
  )

# Handling errors in transformations with ifelse()
df <- data.frame(x = c(1, 2, -1, 4))
df |>
  mutate(
    log_x = ifelse(x > 0, log(x), NA)
  )

# Apply mean to all numeric columns
mtcars |>
  summarise(across(where(is.numeric), mean))

# Apply multiple functions
mtcars |>
  summarise(across(mpg:wt, list(mean = mean, sd = sd)))

# Rename with validation using dplyr::rename_with()
mtcars |>
  rename_with(toupper, starts_with("m")) |>
  head()

# Using rename() with existing column validation
mtcars |>
  rename(
    miles_per_gallon = mpg,
    cylinders = cyl
  ) |>
  head()
Common Questions
How do I select columns by name pattern?
Use starts_with(), ends_with(), or contains():

r
df %>% select(starts_with("col"))
How do I apply a function to all numeric columns?
Use across() with where(is.numeric):

r
df %>% summarise(across(where(is.numeric), mean))
How do I handle missing values in summarise?
Use na.rm = TRUE:

r
df %>% summarise(across(where(is.numeric), mean, na.rm = TRUE))
How do I create conditional variables?
Use case_when() for multiple conditions:

r
df %>%
  mutate(
    category = case_when(
      value < 0 ~ "Negative",
      value == 0 ~ "Zero",
      value > 0 ~ "Positive"
    )
  )
How do I filter case-insensitively?
Use tolower() or toupper():

r
df %>% filter(tolower(name) == "john")
How do I handle errors in transformations?
Use ifelse() or tryCatch():

r
df %>% mutate(log_x = ifelse(x > 0, log(x), NA))
Contributing
We welcome contributions to dplyr! Whether you're:

Reporting bugs

Suggesting features

Improving documentation

Submitting code changes

Adding examples

Please read our contributing guidelines and code of conduct.

Beginner-friendly contributions
Here are some areas where new contributors can make meaningful contributions:

Documentation improvements: Add examples, clarify explanations, fix typos

README enhancements: Add usage examples, improve organization

Vignette updates: Add common questions, clarify concepts

Code comments: Add explanatory comments in complex functions

Small bug fixes: Address edge cases in data manipulation functions

Contribution checklist
□ Read the contributing guidelines
□ Check existing issues and PRs
□ Write clear commit messages
□ Add tests for new features
□ Update documentation
□ Follow tidyverse style guide
Getting help
If you encounter a clear bug, please file an issue with a minimal
reproducible example on
GitHub. For questions and
other discussion, please use forum.posit.co.

For learning resources:

Data transformation with dplyr

dplyr vignettes

Cheat sheet

Code of conduct
Please note that this project is released with a Contributor Code of
Conduct. By participating
in this project you agree to abide by its terms.

Resources
Website: dplyr.tidyverse.org

GitHub: github.com/tidyverse/dplyr

Issues: github.com/tidyverse/dplyr/issues

Discussions: github.com/tidyverse/dplyr/discussions

License
dplyr is licensed under the MIT License. See the LICENSE file for details.

Citation
If you use dplyr in your work, please cite it:

bibtex
@Manual{,
  title = {dplyr: A Grammar of Data Manipulation},
  author = {Hadley Wickham and Romain François and Lionel Henry and Kirill Müller},
  year = {2023},
  note = {R package version 1.1.0},
  url = {https://CRAN.R-project.org/package=dplyr},
}
