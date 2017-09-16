
<!-- README.md is generated from README.Rmd. Please edit that file -->
dplyr <img src="man/figures/logo.png" align="right" />
======================================================

[![Build Status](https://travis-ci.org/tidyverse/dplyr.svg?branch=master)](https://travis-ci.org/tidyverse/dplyr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tidyverse/dplyr?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverse/dplyr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dplyr)](http://cran.r-project.org/package=dplyr) [![Coverage Status](https://codecov.io/gh/tidyverse/dplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/tidyverse/dplyr?branch=master)

Overview
--------

dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges:

-   `mutate()` adds new variables that are functions of existing variables
-   `select()` picks variables based on their names.
-   `filter()` picks cases based on their values.
-   `summarise()` reduces multiple values down to a single summary.
-   `arrange()` changes the ordering of the rows.

These all combine naturally with `group_by()` which allows you to perform any operation "by group". You can learn more about them in `vignette("dplyr")`. As well as these single-table verbs, dplyr also provides a variety of two-table verbs, which you can learn about in `vignette("two-table")`.

dplyr is designed to abstract over how the data is stored. That means as well as working with local data frames, you can also work with remote database tables, using exactly the same R code. Install the dbplyr package then read `vignette("databases", package = "dbplyr")`.

If you are new to dplyr, the best place to start is the [data import chapter](http://r4ds.had.co.nz/transform.html) in R for data science.

Installation
------------

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
install.packages("dplyr")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
```

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/tidyverse/dplyr/issues). For questions and other discussion, please use the [manipulatr mailing list](https://groups.google.com/group/manipulatr).

Usage
-----

``` r
library(dplyr)

starwars %>% 
  filter(species == "Droid")
#> # A tibble: 5 x 13
#>   name  height  mass hair… skin… eye_… birt… gend… home… spec… films vehi…
#>   <chr>  <int> <dbl> <chr> <chr> <chr> <dbl> <chr> <chr> <chr> <lis> <lis>
#> 1 C-3PO    167  75.0 <NA>  gold  yell… 112   <NA>  Tato… Droid <chr… <chr…
#> 2 R2-D2     96  32.0 <NA>  "whi… red    33.0 <NA>  Naboo Droid <chr… <chr…
#> 3 R5-D4     97  32.0 <NA>  "whi… red    NA   <NA>  Tato… Droid <chr… <chr…
#> 4 IG-88    200 140   none  metal red    15.0 none  <NA>  Droid <chr… <chr…
#> 5 BB8       NA  NA   none  none  black  NA   none  <NA>  Droid <chr… <chr…
#> # ... with 1 more variable: starships <list>

starwars %>% 
  select(name, ends_with("color"))
#> # A tibble: 87 x 4
#>   name             hair_color skin_color    eye_color
#>   <chr>            <chr>      <chr>         <chr>    
#> 1 "Luke Skywalker" blond      fair          blue     
#> 2 C-3PO            <NA>       gold          yellow   
#> 3 R2-D2            <NA>       "white, blue" red      
#> 4 "Darth Vader"    none       white         yellow   
#> 5 "Leia Organa"    brown      light         brown    
#> # ... with 82 more rows

starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)
#> # A tibble: 87 x 4
#>   name             height  mass   bmi
#>   <chr>             <int> <dbl> <dbl>
#> 1 "Luke Skywalker"    172  77.0  26.0
#> 2 C-3PO               167  75.0  26.9
#> 3 R2-D2                96  32.0  34.7
#> 4 "Darth Vader"       202 136    33.3
#> 5 "Leia Organa"       150  49.0  21.8
#> # ... with 82 more rows

starwars %>% 
  arrange(desc(mass))
#> # A tibble: 87 x 13
#>   name   heig…  mass hair… skin… eye_… birt… gend… home… spec… films vehi…
#>   <chr>  <int> <dbl> <chr> <chr> <chr> <dbl> <chr> <chr> <chr> <lis> <lis>
#> 1 "Jabb…   175  1358 <NA>  "gre… oran… 600   herm… "Nal… Hutt  <chr… <chr…
#> 2 Griev…   216   159 none  "bro… "gre…  NA   male  Kalee Kale… <chr… <chr…
#> 3 IG-88    200   140 none  metal red    15.0 none  <NA>  Droid <chr… <chr…
#> 4 "Dart…   202   136 none  white yell…  41.9 male  Tato… Human <chr… <chr…
#> 5 Tarff…   234   136 brown brown blue   NA   male  Kash… Wook… <chr… <chr…
#> # ... with 82 more rows, and 1 more variable: starships <list>

starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1)
#> # A tibble: 9 x 3
#>   species      n  mass
#>   <chr>    <int> <dbl>
#> 1 Droid        5  69.8
#> 2 Gungan       3  74.0
#> 3 Human       35  82.8
#> 4 Kaminoan     2  88.0
#> 5 Mirialan     2  53.1
#> # ... with 4 more rows
```
