# Set operations

Perform set operations using the rows of a data frame.

- `intersect(x, y)` finds all rows in both `x` and `y`.

- `union(x, y)` finds all rows in either `x` or `y`, excluding
  duplicates.

- `union_all(x, y)` finds all rows in either `x` or `y`, including
  duplicates.

- `setdiff(x, y)` finds all rows in `x` that aren't in `y`.

- `symdiff(x, y)` computes the symmetric difference, i.e. all rows in
  `x` that aren't in `y` and all rows in `y` that aren't in `x`.

- `setequal(x, y)` returns `TRUE` if `x` and `y` contain the same rows
  (ignoring order).

Note that `intersect()`, `union()`, `setdiff()`, and `symdiff()` remove
duplicates in `x` and `y`.

## Usage

``` r
intersect(x, y, ...)

union(x, y, ...)

union_all(x, y, ...)

setdiff(x, y, ...)

setequal(x, y, ...)

symdiff(x, y, ...)
```

## Arguments

- x, y:

  Pair of compatible data frames. A pair of data frames is compatible if
  they have the same column names (possibly in different orders) and
  compatible types.

- ...:

  These dots are for future extensions and must be empty.

## Base functions

`intersect()`, `union()`, `setdiff()`, and `setequal()` override the
base functions of the same name in order to make them generic. The
existing behaviour for vectors is preserved by providing default methods
that call the base functions.

## Examples

``` r
df1 <- tibble(x = 1:3)
df2 <- tibble(x = 3:5)

intersect(df1, df2)
#> # A tibble: 1 × 1
#>       x
#>   <int>
#> 1     3
union(df1, df2)
#> # A tibble: 5 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
union_all(df1, df2)
#> # A tibble: 6 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 4     3
#> 5     4
#> 6     5
setdiff(df1, df2)
#> # A tibble: 2 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
setdiff(df2, df1)
#> # A tibble: 2 × 1
#>       x
#>   <int>
#> 1     4
#> 2     5
symdiff(df1, df2)
#> # A tibble: 4 × 1
#>       x
#>   <int>
#> 1     1
#> 2     2
#> 3     4
#> 4     5

setequal(df1, df2)
#> [1] FALSE
setequal(df1, df1[3:1, ])
#> [1] TRUE

# Note that the following functions remove pre-existing duplicates:
df1 <- tibble(x = c(1:3, 3, 3))
df2 <- tibble(x = c(3:5, 5))

intersect(df1, df2)
#> # A tibble: 1 × 1
#>       x
#>   <dbl>
#> 1     3
union(df1, df2)
#> # A tibble: 5 × 1
#>       x
#>   <dbl>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
setdiff(df1, df2)
#> # A tibble: 2 × 1
#>       x
#>   <dbl>
#> 1     1
#> 2     2
symdiff(df1, df2)
#> # A tibble: 4 × 1
#>       x
#>   <dbl>
#> 1     1
#> 2     2
#> 3     4
#> 4     5
```
