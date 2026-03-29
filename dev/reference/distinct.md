# Keep distinct/unique rows

Keep only unique/distinct rows from a data frame. This is similar to
[`unique.data.frame()`](https://rdrr.io/r/base/unique.html) but
considerably faster.

## Usage

``` r
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Optional variables to use when determining uniqueness. If there are
  multiple rows for a given combination of inputs, only the first row
  will be preserved. If omitted, will use all variables in the data
  frame.

- .keep_all:

  If `TRUE`, keep all variables in `.data`. If a combination of `...` is
  not distinct, this keeps the first row of values.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are a subset of the input but appear in the same order.

- Columns are not modified if `...` is empty or `.keep_all` is `TRUE`.
  Otherwise, `distinct()` first calls
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) to
  create new columns.

- Groups are not modified.

- Data frame attributes are preserved.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/distinct.tbl_lazy.html)),
dplyr (`data.frame`) .

## Examples

``` r
df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)
nrow(df)
#> [1] 100
nrow(distinct(df))
#> [1] 63
nrow(distinct(df, x, y))
#> [1] 63

distinct(df, x)
#> # A tibble: 10 × 1
#>        x
#>    <int>
#>  1    10
#>  2     5
#>  3     8
#>  4     3
#>  5     2
#>  6     6
#>  7     4
#>  8     1
#>  9     7
#> 10     9
distinct(df, y)
#> # A tibble: 10 × 1
#>        y
#>    <int>
#>  1     7
#>  2    10
#>  3     4
#>  4     8
#>  5     9
#>  6     5
#>  7     6
#>  8     1
#>  9     2
#> 10     3

# You can choose to keep all other variables as well
distinct(df, x, .keep_all = TRUE)
#> # A tibble: 10 × 2
#>        x     y
#>    <int> <int>
#>  1    10     7
#>  2     5    10
#>  3     8     8
#>  4     3     9
#>  5     2     9
#>  6     6     1
#>  7     4    10
#>  8     1    10
#>  9     7     5
#> 10     9     4
distinct(df, y, .keep_all = TRUE)
#> # A tibble: 10 × 2
#>        x     y
#>    <int> <int>
#>  1    10     7
#>  2     5    10
#>  3     5     4
#>  4     8     8
#>  5     3     9
#>  6    10     5
#>  7    10     6
#>  8     6     1
#>  9     7     2
#> 10     5     3

# You can also use distinct on computed variables
distinct(df, diff = abs(x - y))
#> # A tibble: 10 × 1
#>     diff
#>    <int>
#>  1     3
#>  2     5
#>  3     1
#>  4     0
#>  5     6
#>  6     7
#>  7     4
#>  8     9
#>  9     2
#> 10     8

# Use `pick()` to select columns with tidy-select
distinct(starwars, pick(contains("color")))
#> # A tibble: 67 × 3
#>    hair_color    skin_color  eye_color
#>    <chr>         <chr>       <chr>    
#>  1 blond         fair        blue     
#>  2 NA            gold        yellow   
#>  3 NA            white, blue red      
#>  4 none          white       yellow   
#>  5 brown         light       brown    
#>  6 brown, grey   light       blue     
#>  7 brown         light       blue     
#>  8 NA            white, red  red      
#>  9 black         light       brown    
#> 10 auburn, white fair        blue-gray
#> # ℹ 57 more rows

# Grouping -------------------------------------------------

df <- tibble(
  g = c(1, 1, 2, 2, 2),
  x = c(1, 1, 2, 1, 2),
  y = c(3, 2, 1, 3, 1)
)
df <- df |> group_by(g)

# With grouped data frames, distinctness is computed within each group
df |> distinct(x)
#> # A tibble: 3 × 2
#> # Groups:   g [2]
#>       g     x
#>   <dbl> <dbl>
#> 1     1     1
#> 2     2     2
#> 3     2     1

# When `...` are omitted, `distinct()` still computes distinctness using
# all variables in the data frame
df |> distinct()
#> # A tibble: 4 × 3
#> # Groups:   g [2]
#>       g     x     y
#>   <dbl> <dbl> <dbl>
#> 1     1     1     3
#> 2     1     1     2
#> 3     2     2     1
#> 4     2     1     3
```
