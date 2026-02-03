# Group input by rows

`rowwise()` allows you to compute on a data frame a row-at-a-time. This
is most useful when a vectorised function doesn't exist.

Most dplyr verbs preserve row-wise grouping. The exception is
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md),
which return a
[grouped_df](https://dplyr.tidyverse.org/reference/grouped_df.md). You
can explicitly ungroup with
[`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.md) or
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
or convert to a
[grouped_df](https://dplyr.tidyverse.org/reference/grouped_df.md) with
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md).

## Usage

``` r
rowwise(data, ...)
```

## Arguments

- data:

  Input data frame.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Variables to be preserved when calling
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md).
  This is typically a set of variables whose combination uniquely
  identify each row.

  **NB**: unlike
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md) you
  can not create new variables here but instead you can select multiple
  variables with (e.g.)
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## Value

A row-wise data frame with class `rowwise_df`. Note that a `rowwise_df`
is implicitly grouped by row, but is not a `grouped_df`.

## List-columns

Because a rowwise has exactly one row per group it offers a small
convenience for working with list-columns. Normally,
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) and
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md) extract a
groups worth of data with `[`. But when you index a list in this way,
you get back another list. When you're working with a `rowwise` tibble,
then dplyr will use `[[` instead of `[` to make your life a little
easier.

## See also

[`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.md) for a
convenient way of creating rowwise data frames with nested data.

## Examples

``` r
df <- tibble(x = runif(6), y = runif(6), z = runif(6))
# Compute the mean of x, y, z in each row
df |> rowwise() |> mutate(m = mean(c(x, y, z)))
#> # A tibble: 6 × 4
#> # Rowwise: 
#>        x     y     z     m
#>    <dbl> <dbl> <dbl> <dbl>
#> 1 0.915  0.507 0.807 0.743
#> 2 0.831  0.181 0.553 0.522
#> 3 0.0458 0.760 0.646 0.484
#> 4 0.456  0.201 0.312 0.323
#> 5 0.265  0.259 0.622 0.382
#> 6 0.305  0.992 0.330 0.542
# use c_across() to more easily select many variables
df |> rowwise() |> mutate(m = mean(c_across(x:z)))
#> # A tibble: 6 × 4
#> # Rowwise: 
#>        x     y     z     m
#>    <dbl> <dbl> <dbl> <dbl>
#> 1 0.915  0.507 0.807 0.743
#> 2 0.831  0.181 0.553 0.522
#> 3 0.0458 0.760 0.646 0.484
#> 4 0.456  0.201 0.312 0.323
#> 5 0.265  0.259 0.622 0.382
#> 6 0.305  0.992 0.330 0.542

# Compute the minimum of x and y in each row
df |> rowwise() |> mutate(m = min(c(x, y, z)))
#> # A tibble: 6 × 4
#> # Rowwise: 
#>        x     y     z      m
#>    <dbl> <dbl> <dbl>  <dbl>
#> 1 0.915  0.507 0.807 0.507 
#> 2 0.831  0.181 0.553 0.181 
#> 3 0.0458 0.760 0.646 0.0458
#> 4 0.456  0.201 0.312 0.201 
#> 5 0.265  0.259 0.622 0.259 
#> 6 0.305  0.992 0.330 0.305 
# In this case you can use an existing vectorised function:
df |> mutate(m = pmin(x, y, z))
#> # A tibble: 6 × 4
#>        x     y     z      m
#>    <dbl> <dbl> <dbl>  <dbl>
#> 1 0.915  0.507 0.807 0.507 
#> 2 0.831  0.181 0.553 0.181 
#> 3 0.0458 0.760 0.646 0.0458
#> 4 0.456  0.201 0.312 0.201 
#> 5 0.265  0.259 0.622 0.259 
#> 6 0.305  0.992 0.330 0.305 
# Where these functions exist they'll be much faster than rowwise
# so be on the lookout for them.

# rowwise() is also useful when doing simulations
params <- tribble(
 ~sim, ~n, ~mean, ~sd,
    1,  1,     1,   1,
    2,  2,     2,   4,
    3,  3,    -1,   2
)
# Here I supply variables to preserve after the computation
params |>
  rowwise(sim) |>
  reframe(z = rnorm(n, mean, sd))
#> # A tibble: 6 × 2
#>     sim      z
#>   <dbl>  <dbl>
#> 1     1  1.01 
#> 2     2  1.85 
#> 3     2  4.90 
#> 4     3 -1.99 
#> 5     3 -0.977
#> 6     3 -0.980

# If you want one row per simulation, put the results in a list()
params |>
  rowwise(sim) |>
  summarise(z = list(rnorm(n, mean, sd)), .groups = "keep")
#> # A tibble: 3 × 2
#> # Groups:   sim [3]
#>     sim z        
#>   <dbl> <list>   
#> 1     1 <dbl [1]>
#> 2     2 <dbl [2]>
#> 3     3 <dbl [3]>
```
