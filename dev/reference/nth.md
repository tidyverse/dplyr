# Extract the first, last, or nth value from a vector

These are useful helpers for extracting a single value from a vector.
They are guaranteed to return a meaningful value, even when the input is
shorter than expected. You can also provide an optional secondary vector
that defines the ordering.

## Usage

``` r
nth(x, n, order_by = NULL, default = NULL, na_rm = FALSE)

first(x, order_by = NULL, default = NULL, na_rm = FALSE)

last(x, order_by = NULL, default = NULL, na_rm = FALSE)
```

## Arguments

- x:

  A vector

- n:

  For `nth()`, a single integer specifying the position. Negative
  integers index from the end (i.e. `-1L` will return the last value in
  the vector).

- order_by:

  An optional vector the same size as `x` used to determine the order.

- default:

  A default value to use if the position does not exist in `x`.

  If `NULL`, the default, a missing value is used.

  If supplied, this must be a single value, which will be cast to the
  type of `x`.

  When `x` is a list , `default` is allowed to be any value. There are
  no type or size restrictions in this case.

- na_rm:

  Should missing values in `x` be removed before extracting the value?

## Value

If `x` is a list, a single element from that list. Otherwise, a vector
the same type as `x` with size 1.

## Details

For most vector types, `first(x)`, `last(x)`, and `nth(x, n)` work like
`x[[1]]`, `x[[length(x)]`, and `x[[n]]`, respectively. The primary
exception is data frames, where they instead retrieve rows, i.e.
`x[1, ]`, `x[nrow(x), ]`, and `x[n, ]`. This is consistent with the
tidyverse/vctrs principle which treats data frames as a vector of rows,
rather than a vector of columns.

## Examples

``` r
x <- 1:10
y <- 10:1

first(x)
#> [1] 1
last(y)
#> [1] 1

nth(x, 1)
#> [1] 1
nth(x, 5)
#> [1] 5
nth(x, -2)
#> [1] 9

# `first()` and `last()` are often useful in `summarise()`
df <- tibble(x = x, y = y)
df |>
  summarise(
    across(x:y, first, .names = "{col}_first"),
    y_last = last(y)
  )
#> # A tibble: 1 × 3
#>   x_first y_first y_last
#>     <int>   <int>  <int>
#> 1       1      10      1

# Selecting a position that is out of bounds returns a default value
nth(x, 11)
#> [1] NA
nth(x, 0)
#> [1] NA

# This out of bounds behavior also applies to empty vectors
first(integer())
#> [1] NA

# You can customize the default value with `default`
nth(x, 11, default = -1L)
#> [1] -1
first(integer(), default = 0L)
#> [1] 0

# `order_by` provides optional ordering
last(x)
#> [1] 10
last(x, order_by = y)
#> [1] 1

# `na_rm` removes missing values before extracting the value
z <- c(NA, NA, 1, 3, NA, 5, NA)
first(z)
#> [1] NA
first(z, na_rm = TRUE)
#> [1] 1
last(z, na_rm = TRUE)
#> [1] 5
nth(z, 3, na_rm = TRUE)
#> [1] 5

# For data frames, these select entire rows
df <- tibble(a = 1:5, b = 6:10)
first(df)
#> # A tibble: 1 × 2
#>       a     b
#>   <int> <int>
#> 1     1     6
nth(df, 4)
#> # A tibble: 1 × 2
#>       a     b
#>   <int> <int>
#> 1     4     9
```
