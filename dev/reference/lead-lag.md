# Compute lagged or leading values

Find the "previous" (`lag()`) or "next" (`lead()`) values in a vector.
Useful for comparing values behind of or ahead of the current values.

## Usage

``` r
lag(x, n = 1L, default = NULL, order_by = NULL, ...)

lead(x, n = 1L, default = NULL, order_by = NULL, ...)
```

## Arguments

- x:

  A vector

- n:

  Positive integer of length 1, giving the number of positions to lag or
  lead by

- default:

  The value used to pad `x` back to its original size after the lag or
  lead has been applied. The default, `NULL`, pads with a missing value.
  If supplied, this must be a vector with size 1, which will be cast to
  the type of `x`.

- order_by:

  An optional secondary vector that defines the ordering to use when
  applying the lag or lead to `x`. If supplied, this must be the same
  size as `x`.

- ...:

  Not used.

## Value

A vector with the same type and size as `x`.

## Examples

``` r
lag(1:5)
#> [1] NA  1  2  3  4
lead(1:5)
#> [1]  2  3  4  5 NA

x <- 1:5
tibble(behind = lag(x), x, ahead = lead(x))
#> # A tibble: 5 × 3
#>   behind     x ahead
#>    <int> <int> <int>
#> 1     NA     1     2
#> 2      1     2     3
#> 3      2     3     4
#> 4      3     4     5
#> 5      4     5    NA

# If you want to look more rows behind or ahead, use `n`
lag(1:5, n = 1)
#> [1] NA  1  2  3  4
lag(1:5, n = 2)
#> [1] NA NA  1  2  3

lead(1:5, n = 1)
#> [1]  2  3  4  5 NA
lead(1:5, n = 2)
#> [1]  3  4  5 NA NA

# If you want to define a value to pad with, use `default`
lag(1:5)
#> [1] NA  1  2  3  4
lag(1:5, default = 0)
#> [1] 0 1 2 3 4

lead(1:5)
#> [1]  2  3  4  5 NA
lead(1:5, default = 6)
#> [1] 2 3 4 5 6

# If the data are not already ordered, use `order_by`
scrambled <- slice_sample(
  tibble(year = 2000:2005, value = (0:5) ^ 2),
  prop = 1
)

wrong <- mutate(scrambled, previous_year_value = lag(value))
arrange(wrong, year)
#> # A tibble: 6 × 3
#>    year value previous_year_value
#>   <int> <dbl>               <dbl>
#> 1  2000     0                   9
#> 2  2001     1                  16
#> 3  2002     4                  25
#> 4  2003     9                  NA
#> 5  2004    16                   4
#> 6  2005    25                   0

right <- mutate(scrambled, previous_year_value = lag(value, order_by = year))
arrange(right, year)
#> # A tibble: 6 × 3
#>    year value previous_year_value
#>   <int> <dbl>               <dbl>
#> 1  2000     0                  NA
#> 2  2001     1                   0
#> 3  2002     4                   1
#> 4  2003     9                   4
#> 5  2004    16                   9
#> 6  2005    25                  16
```
