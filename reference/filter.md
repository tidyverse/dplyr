# Keep or drop rows that match a condition

These functions are used to subset a data frame, applying the
expressions in `...` to determine which rows should be kept (for
`filter()`) or dropped ( for `filter_out()`).

Multiple conditions can be supplied separated by a comma. These will be
combined with the `&` operator. To combine comma separated conditions
using `|` instead, wrap them in
[`when_any()`](https://dplyr.tidyverse.org/reference/when-any-all.md).

Both `filter()` and `filter_out()` treat `NA` like `FALSE`. This subtle
behavior can impact how you write your conditions when missing values
are involved. See the section on `Missing values` for important details
and examples.

## Usage

``` r
filter(.data, ..., .by = NULL, .preserve = FALSE)

filter_out(.data, ..., .by = NULL, .preserve = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical vector, defined in terms of the
  variables in `.data`. If multiple expressions are included, they are
  combined with the `&` operator. To combine expressions using `|`
  instead, wrap them in
  [`when_any()`](https://dplyr.tidyverse.org/reference/when-any-all.md).
  Only rows for which all expressions evaluate to `TRUE` are kept (for
  `filter()`) or dropped (for `filter_out()`).

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md). For
  details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.md).

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

## Missing values

Both `filter()` and `filter_out()` treat `NA` like `FALSE`. This results
in the following behavior:

- `filter()` *drops* both `NA` and `FALSE`.

- `filter_out()` *keeps* both `NA` and `FALSE`.

This means that
`filter(data, <conditions>) + filter_out(data, <conditions>)` captures
every row within `data` exactly once.

The `NA` handling of these functions has been designed to match your
*intent*. When your intent is to keep rows, use `filter()`. When your
intent is to drop rows, use `filter_out()`.

For example, if your goal with this `cars` data is to "drop rows where
the `class` is suv", then you might write this in one of two ways:

    cars <- tibble(class = c("suv", NA, "coupe"))
    cars
    #> # A tibble: 3 x 1
    #>   class
    #>   <chr>
    #> 1 suv
    #> 2 <NA>
    #> 3 coupe

    cars |> filter(class != "suv")
    #> # A tibble: 1 x 1
    #>   class
    #>   <chr>
    #> 1 coupe

    cars |> filter_out(class == "suv")
    #> # A tibble: 2 x 1
    #>   class
    #>   <chr>
    #> 1 <NA>
    #> 2 coupe

Note how `filter()` drops the `NA` rows even though our goal was only to
drop `"suv"` rows, but `filter_out()` matches our intuition.

To generate the correct result with `filter()`, you'd need to use:

    cars |> filter(class != "suv" | is.na(class))
    #> # A tibble: 2 x 1
    #>   class
    #>   <chr>
    #> 1 <NA>
    #> 2 coupe

This quickly gets unwieldy when multiple conditions are involved.

In general, if you find yourself:

- Using "negative" operators like `!=` or `!`

- Adding in `NA` handling like `| is.na(col)` or `& !is.na(col)`

then you should consider if swapping to the other filtering variant
would make your conditions simpler.

### Comparison to base subsetting

Base subsetting with `[` doesn't treat `NA` like `TRUE` or `FALSE`.
Instead, it generates a fully missing row, which is different from how
both `filter()` and `filter_out()` work.

    cars <- tibble(class = c("suv", NA, "coupe"), mpg = c(10, 12, 14))
    cars
    #> # A tibble: 3 x 2
    #>   class   mpg
    #>   <chr> <dbl>
    #> 1 suv      10
    #> 2 <NA>     12
    #> 3 coupe    14

    cars[cars$class == "suv",]
    #> # A tibble: 2 x 2
    #>   class   mpg
    #>   <chr> <dbl>
    #> 1 suv      10
    #> 2 <NA>     NA

    cars |> filter(class == "suv")
    #> # A tibble: 1 x 2
    #>   class   mpg
    #>   <chr> <dbl>
    #> 1 suv      10

## Useful filter functions

There are many functions and operators that are useful when constructing
the expressions used to filter the data:

- [`==`](https://rdrr.io/r/base/Comparison.html), `>`, `>=` etc

- `&`, [`|`](https://rdrr.io/r/base/Logic.html),
  [`!`](https://rdrr.io/r/base/Logic.html),
  [`xor()`](https://rdrr.io/r/base/Logic.html)

- [`is.na()`](https://rdrr.io/r/base/NA.html)

- [`between()`](https://dplyr.tidyverse.org/reference/between.md),
  [`near()`](https://dplyr.tidyverse.org/reference/near.md)

- [`when_any()`](https://dplyr.tidyverse.org/reference/when-any-all.md),
  [`when_all()`](https://dplyr.tidyverse.org/reference/when-any-all.md)

## Grouped tibbles

Because filtering expressions are computed within groups, they may yield
different results on grouped tibbles. This will be the case as soon as
an aggregating, lagging, or ranking function is involved. Compare this
ungrouped filtering:

    starwars |> filter(mass > mean(mass, na.rm = TRUE))

With the grouped equivalent:

    starwars |> filter(mass > mean(mass, na.rm = TRUE), .by = gender)

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
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.md),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md)

## Examples

``` r
# Filtering for one criterion
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

# Filtering for multiple criteria within a single logical expression
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

# Multiple comma separated expressions are combined using `&`
starwars |> filter(hair_color == "none", eye_color == "black")
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

# To combine comma separated expressions using `|` instead, use `when_any()`
starwars |> filter(when_any(hair_color == "none", eye_color == "black"))
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

# Filtering out to drop rows
filter_out(starwars, hair_color == "none")
#> # A tibble: 49 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke …    172    77 blond      fair       blue            19   male 
#>  2 C-3PO     167    75 NA         gold       yellow         112   none 
#>  3 R2-D2      96    32 NA         white, bl… red             33   none 
#>  4 Leia …    150    49 brown      light      brown           19   fema…
#>  5 Owen …    178   120 brown, gr… light      blue            52   male 
#>  6 Beru …    165    75 brown      light      blue            47   fema…
#>  7 R5-D4      97    32 NA         white, red red             NA   none 
#>  8 Biggs…    183    84 black      light      brown           24   male 
#>  9 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#> 10 Anaki…    188    84 blond      fair       blue            41.9 male 
#> # ℹ 39 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# When filtering out, it can be useful to first interactively filter for the
# rows you want to drop, just to double check that you've written the
# conditions correctly. Then, just change `filter()` to `filter_out()`.
filter(starwars, mass > 1000, eye_color == "orange")
#> # A tibble: 1 × 14
#>   name    height  mass hair_color skin_color eye_color birth_year sex  
#>   <chr>    <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#> 1 Jabba …    175  1358 NA         green-tan… orange           600 herm…
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
filter_out(starwars, mass > 1000, eye_color == "orange")
#> # A tibble: 86 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke …    172    77 blond      fair       blue            19   male 
#>  2 C-3PO     167    75 NA         gold       yellow         112   none 
#>  3 R2-D2      96    32 NA         white, bl… red             33   none 
#>  4 Darth…    202   136 none       white      yellow          41.9 male 
#>  5 Leia …    150    49 brown      light      brown           19   fema…
#>  6 Owen …    178   120 brown, gr… light      blue            52   male 
#>  7 Beru …    165    75 brown      light      blue            47   fema…
#>  8 R5-D4      97    32 NA         white, red red             NA   none 
#>  9 Biggs…    183    84 black      light      brown           24   male 
#> 10 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#> # ℹ 76 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# The filtering operation may yield different results on grouped
# tibbles because the expressions are computed within groups.
#
# The following keeps rows where `mass` is greater than the
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

# Whereas this keeps rows with `mass` greater than the per `gender`
# average:
starwars |> filter(mass > mean(mass, na.rm = TRUE), .by = gender)
#> # A tibble: 15 × 14
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

# If you find yourself trying to use a `filter()` to drop rows, then
# you should consider if switching to `filter_out()` can simplify your
# conditions. For example, to drop blond individuals, you might try:
starwars |> filter(hair_color != "blond")
#> # A tibble: 79 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Darth…    202   136 none       white      yellow          41.9 male 
#>  2 Leia …    150    49 brown      light      brown           19   fema…
#>  3 Owen …    178   120 brown, gr… light      blue            52   male 
#>  4 Beru …    165    75 brown      light      blue            47   fema…
#>  5 Biggs…    183    84 black      light      brown           24   male 
#>  6 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#>  7 Wilhu…    180    NA auburn, g… fair       blue            64   male 
#>  8 Chewb…    228   112 brown      unknown    blue           200   male 
#>  9 Han S…    180    80 brown      fair       brown           29   male 
#> 10 Wedge…    170    77 brown      fair       hazel           21   male 
#> # ℹ 69 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# But this also drops rows with an `NA` hair color! To retain those:
starwars |> filter(hair_color != "blond" | is.na(hair_color))
#> # A tibble: 84 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 C-3PO     167    75 NA         gold       yellow         112   none 
#>  2 R2-D2      96    32 NA         white, bl… red             33   none 
#>  3 Darth…    202   136 none       white      yellow          41.9 male 
#>  4 Leia …    150    49 brown      light      brown           19   fema…
#>  5 Owen …    178   120 brown, gr… light      blue            52   male 
#>  6 Beru …    165    75 brown      light      blue            47   fema…
#>  7 R5-D4      97    32 NA         white, red red             NA   none 
#>  8 Biggs…    183    84 black      light      brown           24   male 
#>  9 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#> 10 Wilhu…    180    NA auburn, g… fair       blue            64   male 
#> # ℹ 74 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# But explicit `NA` handling like this can quickly get unwieldy, especially
# with multiple conditions. Since your intent was to specify rows to drop
# rather than rows to keep, use `filter_out()`. This also removes the need
# for any explicit `NA` handling.
starwars |> filter_out(hair_color == "blond")
#> # A tibble: 84 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 C-3PO     167    75 NA         gold       yellow         112   none 
#>  2 R2-D2      96    32 NA         white, bl… red             33   none 
#>  3 Darth…    202   136 none       white      yellow          41.9 male 
#>  4 Leia …    150    49 brown      light      brown           19   fema…
#>  5 Owen …    178   120 brown, gr… light      blue            52   male 
#>  6 Beru …    165    75 brown      light      blue            47   fema…
#>  7 R5-D4      97    32 NA         white, red red             NA   none 
#>  8 Biggs…    183    84 black      light      brown           24   male 
#>  9 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#> 10 Wilhu…    180    NA auburn, g… fair       blue            64   male 
#> # ℹ 74 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>

# To refer to column names that are stored as strings, use the `.data`
# pronoun:
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
