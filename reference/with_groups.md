# Perform an operation with temporary groups

**\[superseded\]**

This was an experimental function that allows you to modify the grouping
variables for a single operation; it is superseded in favour of using
the `.by` argument to individual verbs.

## Usage

``` r
with_groups(.data, .groups, .f, ...)
```

## Arguments

- .data:

  A data frame

- .groups:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  One or more variables to group by. Unlike
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.md), you
  can only group by existing variables, and you can use tidy-select
  syntax like `c(x, y, z)` to select multiple variables.

  Use `NULL` to temporarily **un**group.

- .f:

  Function to apply to regrouped data. Supports purrr-style `~` syntax

- ...:

  Additional arguments passed on to `...`.

## Examples

``` r
df <- tibble(g = c(1, 1, 2, 2, 3), x = runif(5))

# Old
df |>
  with_groups(g, mutate, x_mean = mean(x))
#> # A tibble: 5 × 3
#>       g     x x_mean
#>   <dbl> <dbl>  <dbl>
#> 1     1 0.990  0.707
#> 2     1 0.424  0.707
#> 3     2 0.244  0.231
#> 4     2 0.217  0.231
#> 5     3 0.689  0.689
# New
df |> mutate(x_mean = mean(x), .by = g)
#> # A tibble: 5 × 3
#>       g     x x_mean
#>   <dbl> <dbl>  <dbl>
#> 1     1 0.990  0.707
#> 2     1 0.424  0.707
#> 3     2 0.244  0.231
#> 4     2 0.217  0.231
#> 5     3 0.689  0.689
```
