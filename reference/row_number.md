# Integer ranking functions

Three ranking functions inspired by SQL2003. They differ primarily in
how they handle ties:

- `row_number()` gives every input a unique rank, so that
  `c(10, 20, 20, 30)` would get ranks `c(1, 2, 3, 4)`. It's equivalent
  to `rank(ties.method = "first")`.

- `min_rank()` gives every tie the same (smallest) value so that
  `c(10, 20, 20, 30)` gets ranks `c(1, 2, 2, 4)`. It's the way that
  ranks are usually computed in sports and is equivalent to
  `rank(ties.method = "min")`.

- `dense_rank()` works like `min_rank()`, but doesn't leave any gaps, so
  that `c(10, 20, 20, 30)` gets ranks `c(1, 2, 2, 3)`.

## Usage

``` r
row_number(x)

min_rank(x)

dense_rank(x)
```

## Arguments

- x:

  A vector to rank

  By default, the smallest values will get the smallest ranks. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.md) to reverse
  the direction so the largest values get the smallest ranks.

  Missing values will be given rank `NA`. Use `coalesce(x, Inf)` or
  `coalesce(x, -Inf)` if you want to treat them as the largest or
  smallest values respectively.

  To rank by multiple columns at once, supply a data frame.

## Value

An integer vector.

## See also

Other ranking functions:
[`ntile()`](https://dplyr.tidyverse.org/reference/ntile.md),
[`percent_rank()`](https://dplyr.tidyverse.org/reference/percent_rank.md)

## Examples

``` r
x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
#> [1]  5  1  4  2  3 NA
min_rank(x)
#> [1]  5  1  4  2  2 NA
dense_rank(x)
#> [1]  4  1  3  2  2 NA

# Ranking functions can be used in `filter()` to select top/bottom rows
df <- data.frame(
  grp = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  x = c(3, 2, 1, 1, 2, 2, 1, 1, 1),
  y = c(1, 3, 2, 3, 2, 2, 4, 1, 2),
  id = 1:9
)
# Always gives exactly 1 row per group
df |> group_by(grp) |> filter(row_number(x) == 1)
#> # A tibble: 3 × 4
#> # Groups:   grp [3]
#>     grp     x     y    id
#>   <dbl> <dbl> <dbl> <int>
#> 1     1     1     2     3
#> 2     2     1     3     4
#> 3     3     1     4     7
# May give more than 1 row if ties
df |> group_by(grp) |> filter(min_rank(x) == 1)
#> # A tibble: 5 × 4
#> # Groups:   grp [3]
#>     grp     x     y    id
#>   <dbl> <dbl> <dbl> <int>
#> 1     1     1     2     3
#> 2     2     1     3     4
#> 3     3     1     4     7
#> 4     3     1     1     8
#> 5     3     1     2     9
# Rank by multiple columns (to break ties) by selecting them with `pick()`
df |> group_by(grp) |> filter(min_rank(pick(x, y)) == 1)
#> # A tibble: 3 × 4
#> # Groups:   grp [3]
#>     grp     x     y    id
#>   <dbl> <dbl> <dbl> <int>
#> 1     1     1     2     3
#> 2     2     1     3     4
#> 3     3     1     1     8
# See slice_min() and slice_max() for another way to tackle the same problem

# You can use row_number() without an argument to refer to the "current"
# row number.
df |> group_by(grp) |> filter(row_number() == 1)
#> # A tibble: 3 × 4
#> # Groups:   grp [3]
#>     grp     x     y    id
#>   <dbl> <dbl> <dbl> <int>
#> 1     1     3     1     1
#> 2     2     1     3     4
#> 3     3     1     4     7

# It's easiest to see what this does with mutate():
df |> group_by(grp) |> mutate(grp_id = row_number())
#> # A tibble: 9 × 5
#> # Groups:   grp [3]
#>     grp     x     y    id grp_id
#>   <dbl> <dbl> <dbl> <int>  <int>
#> 1     1     3     1     1      1
#> 2     1     2     3     2      2
#> 3     1     1     2     3      3
#> 4     2     1     3     4      1
#> 5     2     2     2     5      2
#> 6     2     2     2     6      3
#> 7     3     1     4     7      1
#> 8     3     1     1     8      2
#> 9     3     1     2     9      3
```
