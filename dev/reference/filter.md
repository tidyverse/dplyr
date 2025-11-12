# Keep rows that match a condition

The `filter()` function is used to subset a data frame, retaining all
rows that satisfy your conditions. To be retained, the row must produce
a value of `TRUE` for all conditions. Note that when a condition
evaluates to `NA` the row will be dropped, unlike base subsetting with
`[`.

## Usage

``` r
filter(.data, ..., .by = NULL, .preserve = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the variables in `.data`. If multiple expressions are included, they
  are combined with the `&` operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/dev/reference/dplyr_by.md).

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are a subset of the input, but appear in the same order.

- Columns are not modified.

- The number of groups may be reduced (if `.preserve` is not `TRUE`).

- Data frame attributes are preserved.

## Details

The `filter()` function is used to subset the rows of `.data`, applying
the expressions in `...` to the column values to determine which rows
should be retained. It can be applied to both grouped and ungrouped data
(see
[`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
and
[`ungroup()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)).
However, dplyr is not yet smart enough to optimise the filtering
operation on grouped datasets that do not need grouped calculations. For
this reason, filtering is often considerably faster on ungrouped data.

## Useful filter functions

There are many functions and operators that are useful when constructing
the expressions used to filter the data:

- [`==`](https://rdrr.io/r/base/Comparison.html), `>`, `>=` etc

- `&`, [`|`](https://rdrr.io/r/base/Logic.html),
  [`!`](https://rdrr.io/r/base/Logic.html),
  [`xor()`](https://rdrr.io/r/base/Logic.html)

- [`is.na()`](https://rdrr.io/r/base/NA.html)

- [`between()`](https://dplyr.tidyverse.org/dev/reference/between.md),
  [`near()`](https://dplyr.tidyverse.org/dev/reference/near.md)

## Grouped tibbles

Because filtering expressions are computed within groups, they may yield
different results on grouped tibbles. This will be the case as soon as
an aggregating, lagging, or ranking function is involved. Compare this
ungrouped filtering:

    starwars |> filter(mass > mean(mass, na.rm = TRUE))

With the grouped equivalent:

    starwars |> group_by(gender) |> filter(mass > mean(mass, na.rm = TRUE))

In the ungrouped version, `filter()` compares the value of `mass` in
each row to the global average (taken over the whole data set), keeping
only the rows with `mass` greater than this global average. In contrast,
the grouped version calculates the average mass separately for each
`gender` group, and keeps rows with `mass` greater than the relevant
within-gender average.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/filter.tbl_lazy.html)),
dplyr (`data.frame`, `ts`) .

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)

## Examples

``` r
# Filtering by one criterion
filter(starwars, species == "Human")
#> # A tibble: 35 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke …    172    77 blond      fair       blue            19   male 
#>  2 Darth…    202   136 none       white      yellow          41.9 male 
#>  3 Leia …    150    49 brown      light      brown           19   fema…
#>  4 Owen …    178   120 brown, gr… light      blue            52   male 
#>  5 Beru …    165    75 brown      light      blue            47   fema…
#>  6 Biggs…    183    84 black      light      brown           24   male 
#>  7 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#>  8 Anaki…    188    84 blond      fair       blue            41.9 male 
#>  9 Wilhu…    180    NA auburn, g… fair       blue            64   male 
#> 10 Han S…    180    80 brown      fair       brown           29   male 
#> # ℹ 25 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
filter(starwars, mass > 1000)
#> # A tibble: 1 × 14
#>   name    height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>    <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Jabba …    175  1358 NA         green-tan… orange           600 herm…
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# Filtering by multiple criteria within a single logical expression
filter(starwars, hair_color == "none" & eye_color == "black")
#> # A tibble: 9 × 14
#>   name    height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>    <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Nien N…    160    68 none       grey       black             NA male 
#> 2 Gasgano    122    NA none       white, bl… black             NA male 
#> 3 Kit Fi…    196    87 none       green      black             NA male 
#> 4 Plo Ko…    188    80 none       orange     black             22 male 
#> 5 Lama Su    229    88 none       grey       black             NA male 
#> 6 Taun We    213    NA none       grey       black             NA fema…
#> 7 Shaak …    178    57 none       red, blue… black             NA fema…
#> 8 Tion M…    206    80 none       grey       black             NA male 
#> 9 BB8         NA    NA none       none       black             NA none 
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
filter(starwars, hair_color == "none" | eye_color == "black")
#> # A tibble: 39 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Darth…    202   136 none       white      yellow          41.9 male 
#>  2 Greedo    173    74 NA         green      black           44   male 
#>  3 IG-88     200   140 none       metal      red             15   none 
#>  4 Bossk     190   113 none       green      red             53   male 
#>  5 Lobot     175    79 none       light      blue            37   male 
#>  6 Ackbar    180    83 none       brown mot… orange          41   male 
#>  7 Nien …    160    68 none       grey       black           NA   male 
#>  8 Nute …    191    90 none       mottled g… red             NA   male 
#>  9 Jar J…    196    66 none       orange     orange          52   male 
#> 10 Roos …    224    82 none       grey       orange          NA   male 
#> # ℹ 29 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# When multiple expressions are used, they are combined using &
filter(starwars, hair_color == "none", eye_color == "black")
#> # A tibble: 9 × 14
#>   name    height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>    <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Nien N…    160    68 none       grey       black             NA male 
#> 2 Gasgano    122    NA none       white, bl… black             NA male 
#> 3 Kit Fi…    196    87 none       green      black             NA male 
#> 4 Plo Ko…    188    80 none       orange     black             22 male 
#> 5 Lama Su    229    88 none       grey       black             NA male 
#> 6 Taun We    213    NA none       grey       black             NA fema…
#> 7 Shaak …    178    57 none       red, blue… black             NA fema…
#> 8 Tion M…    206    80 none       grey       black             NA male 
#> 9 BB8         NA    NA none       none       black             NA none 
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>


# The filtering operation may yield different results on grouped
# tibbles because the expressions are computed within groups.
#
# The following filters rows where `mass` is greater than the
# global average:
starwars |> filter(mass > mean(mass, na.rm = TRUE))
#> # A tibble: 10 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Darth…    202   136 none       white      yellow          41.9 male 
#>  2 Owen …    178   120 brown, gr… light      blue            52   male 
#>  3 Chewb…    228   112 brown      unknown    blue           200   male 
#>  4 Jabba…    175  1358 NA         green-tan… orange         600   herm…
#>  5 Jek T…    180   110 brown      fair       blue            NA   NA   
#>  6 IG-88     200   140 none       metal      red             15   none 
#>  7 Bossk     190   113 none       green      red             53   male 
#>  8 Dexte…    198   102 none       brown      yellow          NA   male 
#>  9 Griev…    216   159 none       brown, wh… green, y…       NA   male 
#> 10 Tarff…    234   136 brown      brown      blue            NA   male 
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# Whereas this keeps rows with `mass` greater than the gender
# average:
starwars |> group_by(gender) |> filter(mass > mean(mass, na.rm = TRUE))
#> # A tibble: 15 × 14
#> # Groups:   gender [3]
#>    name  height   mass hair_color skin_color eye_color birth_year sex  
#>    <chr>  <int>  <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Dart…    202  136   none       white      yellow          41.9 male 
#>  2 Owen…    178  120   brown, gr… light      blue            52   male 
#>  3 Beru…    165   75   brown      light      blue            47   fema…
#>  4 Chew…    228  112   brown      unknown    blue           200   male 
#>  5 Jabb…    175 1358   NA         green-tan… orange         600   herm…
#>  6 Jek …    180  110   brown      fair       blue            NA   NA   
#>  7 IG-88    200  140   none       metal      red             15   none 
#>  8 Bossk    190  113   none       green      red             53   male 
#>  9 Ayla…    178   55   none       blue       hazel           48   fema…
#> 10 Greg…    185   85   black      dark       brown           NA   NA   
#> 11 Lumi…    170   56.2 black      yellow     blue            58   fema…
#> 12 Zam …    168   55   blonde     fair, gre… yellow          NA   fema…
#> 13 Shaa…    178   57   none       red, blue… black           NA   fema…
#> 14 Grie…    216  159   none       brown, wh… green, y…       NA   male 
#> 15 Tarf…    234  136   brown      brown      blue            NA   male 
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>


# To refer to column names that are stored as strings, use the `.data` pronoun:
vars <- c("mass", "height")
cond <- c(80, 150)
starwars |>
  filter(
    .data[[vars[[1]]]] > cond[[1]],
    .data[[vars[[2]]]] > cond[[2]]
  )
#> # A tibble: 21 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Darth…    202   136 none       white      yellow          41.9 male 
#>  2 Owen …    178   120 brown, gr… light      blue            52   male 
#>  3 Biggs…    183    84 black      light      brown           24   male 
#>  4 Anaki…    188    84 blond      fair       blue            41.9 male 
#>  5 Chewb…    228   112 brown      unknown    blue           200   male 
#>  6 Jabba…    175  1358 NA         green-tan… orange         600   herm…
#>  7 Jek T…    180   110 brown      fair       blue            NA   NA   
#>  8 IG-88     200   140 none       metal      red             15   none 
#>  9 Bossk     190   113 none       green      red             53   male 
#> 10 Ackbar    180    83 none       brown mot… orange          41   male 
#> # ℹ 11 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
# Learn more in ?rlang::args_data_masking
```
