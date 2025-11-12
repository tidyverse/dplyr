# Proportional ranking functions

These two ranking functions implement two slightly different ways to
compute a percentile. For each `x_i` in `x`:

- `cume_dist(x)` counts the total number of values less than or equal to
  `x_i`, and divides it by the number of observations.

- `percent_rank(x)` counts the total number of values less than `x_i`,
  and divides it by the number of observations minus 1.

In both cases, missing values are ignored when counting the number of
observations.

## Usage

``` r
percent_rank(x)

cume_dist(x)
```

## Arguments

- x:

  A vector to rank

  By default, the smallest values will get the smallest ranks. Use
  [`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md) to
  reverse the direction so the largest values get the smallest ranks.

  Missing values will be given rank `NA`. Use `coalesce(x, Inf)` or
  `coalesce(x, -Inf)` if you want to treat them as the largest or
  smallest values respectively.

  To rank by multiple columns at once, supply a data frame.

## Value

A numeric vector containing a proportion.

## See also

Other ranking functions:
[`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md),
[`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md)

## Examples

``` r
x <- c(5, 1, 3, 2, 2)

cume_dist(x)
#> [1] 1.0 0.2 0.8 0.6 0.6
percent_rank(x)
#> [1] 1.00 0.00 0.75 0.25 0.25

# You can understand what's going on by computing it by hand
sapply(x, function(xi) sum(x <= xi) / length(x))
#> [1] 1.0 0.2 0.8 0.6 0.6
sapply(x, function(xi) sum(x < xi)  / (length(x) - 1))
#> [1] 1.00 0.00 0.75 0.25 0.25
# The real computations are a little more complex in order to
# correctly deal with missing values
```
