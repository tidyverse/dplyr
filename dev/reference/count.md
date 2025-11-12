# Count the observations in each group

`count()` lets you quickly count the unique values of one or more
variables: `df |> count(a, b)` is roughly equivalent to
`df |> group_by(a, b) |> summarise(n = n())`. `count()` is paired with
`tally()`, a lower-level helper that is equivalent to
`df |> summarise(n = n())`. Supply `wt` to perform weighted counts,
switching the summary from `n = n()` to `n = sum(wt)`.

`add_count()` and `add_tally()` are equivalents to `count()` and
`tally()` but use
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)
instead of
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
so that they add a new column with group-wise counts.

## Usage

``` r
count(x, ..., wt = NULL, sort = FALSE, name = NULL)

# S3 method for class 'data.frame'
count(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = group_by_drop_default(x)
)

tally(x, wt = NULL, sort = FALSE, name = NULL)

add_count(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated())

add_tally(x, wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- .drop:

  Handling of factor levels that don't appear in the data, passed on to
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).

  For `count()`: if `FALSE` will include counts for empty groups (i.e.
  for levels of factors that don't exist in the data).

  **\[defunct\]** For `add_count()`: defunct since it can't actually
  affect the output.

## Value

An object of the same type as `.data`. `count()` and `add_count()` group
transiently, so the output has the same groups as the input.

## Examples

``` r
# count() is a convenient way to get a sense of the distribution of
# values in a dataset
starwars |> count(species)
#> # A tibble: 38 × 2
#>    species       n
#>    <chr>     <int>
#>  1 Aleena        1
#>  2 Besalisk      1
#>  3 Cerean        1
#>  4 Chagrian      1
#>  5 Clawdite      1
#>  6 Droid         6
#>  7 Dug           1
#>  8 Ewok          1
#>  9 Geonosian     1
#> 10 Gungan        3
#> # ℹ 28 more rows
starwars |> count(species, sort = TRUE)
#> # A tibble: 38 × 2
#>    species      n
#>    <chr>    <int>
#>  1 Human       35
#>  2 Droid        6
#>  3 NA           4
#>  4 Gungan       3
#>  5 Kaminoan     2
#>  6 Mirialan     2
#>  7 Twi'lek      2
#>  8 Wookiee      2
#>  9 Zabrak       2
#> 10 Aleena       1
#> # ℹ 28 more rows
starwars |> count(sex, gender, sort = TRUE)
#> # A tibble: 6 × 3
#>   sex            gender        n
#>   <chr>          <chr>     <int>
#> 1 male           masculine    60
#> 2 female         feminine     16
#> 3 none           masculine     5
#> 4 NA             NA            4
#> 5 hermaphroditic masculine     1
#> 6 none           feminine      1
starwars |> count(birth_decade = round(birth_year, -1))
#> # A tibble: 15 × 2
#>    birth_decade     n
#>           <dbl> <int>
#>  1           10     1
#>  2           20     6
#>  3           30     4
#>  4           40     6
#>  5           50     8
#>  6           60     4
#>  7           70     4
#>  8           80     2
#>  9           90     3
#> 10          100     1
#> 11          110     1
#> 12          200     1
#> 13          600     1
#> 14          900     1
#> 15           NA    44

# use the `wt` argument to perform a weighted count. This is useful
# when the data has already been aggregated once
df <- tribble(
  ~name,    ~gender,   ~runs,
  "Max",    "male",       10,
  "Sandra", "female",      1,
  "Susan",  "female",      4
)
# counts rows:
df |> count(gender)
#> # A tibble: 2 × 2
#>   gender     n
#>   <chr>  <int>
#> 1 female     2
#> 2 male       1
# counts runs:
df |> count(gender, wt = runs)
#> # A tibble: 2 × 2
#>   gender     n
#>   <chr>  <dbl>
#> 1 female     5
#> 2 male      10

# When factors are involved, `.drop = FALSE` can be used to retain factor
# levels that don't appear in the data
df2 <- tibble(
  id = 1:5,
  type = factor(c("a", "c", "a", NA, "a"), levels = c("a", "b", "c"))
)
df2 |> count(type)
#> # A tibble: 3 × 2
#>   type      n
#>   <fct> <int>
#> 1 a         3
#> 2 c         1
#> 3 NA        1
df2 |> count(type, .drop = FALSE)
#> # A tibble: 4 × 2
#>   type      n
#>   <fct> <int>
#> 1 a         3
#> 2 b         0
#> 3 c         1
#> 4 NA        1

# Or, using `group_by()`:
df2 |> group_by(type, .drop = FALSE) |> count()
#> # A tibble: 4 × 2
#> # Groups:   type [4]
#>   type      n
#>   <fct> <int>
#> 1 a         3
#> 2 b         0
#> 3 c         1
#> 4 NA        1

# tally() is a lower-level function that assumes you've done the grouping
starwars |> tally()
#> # A tibble: 1 × 1
#>       n
#>   <int>
#> 1    87
starwars |> group_by(species) |> tally()
#> # A tibble: 38 × 2
#>    species       n
#>    <chr>     <int>
#>  1 Aleena        1
#>  2 Besalisk      1
#>  3 Cerean        1
#>  4 Chagrian      1
#>  5 Clawdite      1
#>  6 Droid         6
#>  7 Dug           1
#>  8 Ewok          1
#>  9 Geonosian     1
#> 10 Gungan        3
#> # ℹ 28 more rows

# both count() and tally() have add_ variants that work like
# mutate() instead of summarise
df |> add_count(gender, wt = runs)
#> # A tibble: 3 × 4
#>   name   gender  runs     n
#>   <chr>  <chr>  <dbl> <dbl>
#> 1 Max    male      10    10
#> 2 Sandra female     1     5
#> 3 Susan  female     4     5
df |> add_tally(wt = runs)
#> # A tibble: 3 × 4
#>   name   gender  runs     n
#>   <chr>  <chr>  <dbl> <dbl>
#> 1 Max    male      10    15
#> 2 Sandra female     1    15
#> 3 Susan  female     4    15
```
