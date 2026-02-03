# Select a subset of columns

`pick()` provides a way to easily select a subset of columns from your
data using [`select()`](https://dplyr.tidyverse.org/reference/select.md)
semantics while inside a
["data-masking"](https://rlang.r-lib.org/reference/args_data_masking.html)
function like
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md) or
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md).
`pick()` returns a data frame containing the selected columns for the
current group.

`pick()` is complementary to
[`across()`](https://dplyr.tidyverse.org/reference/across.md):

- With `pick()`, you typically apply a function to the full data frame.

- With [`across()`](https://dplyr.tidyverse.org/reference/across.md),
  you typically apply a function to each column.

## Usage

``` r
pick(...)
```

## Arguments

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>

  Columns to pick.

  You can't pick grouping columns because they are already automatically
  handled by the verb (i.e.
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) or
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md)).

## Value

A tibble containing the selected columns for the current group.

## Details

Theoretically, `pick()` is intended to be replaceable with an equivalent
call to
[`tibble()`](https://tibble.tidyverse.org/reference/tibble.html). For
example, `pick(a, c)` could be replaced with `tibble(a = a, c = c)`, and
`pick(everything())` on a data frame with cols `a`, `b`, and `c` could
be replaced with `tibble(a = a, b = b, c = c)`. `pick()` specially
handles the case of an empty selection by returning a 1 row, 0 column
tibble, so an exact replacement is more like:

    size <- vctrs::vec_size_common(..., .absent = 1L)
    out <- vctrs::vec_recycle_common(..., .size = size)
    tibble::new_tibble(out, nrow = size)

## See also

[`across()`](https://dplyr.tidyverse.org/reference/across.md)

## Examples

``` r
df <- tibble(
  x = c(3, 2, 2, 2, 1),
  y = c(0, 2, 1, 1, 4),
  z1 = c("a", "a", "a", "b", "a"),
  z2 = c("c", "d", "d", "a", "c")
)
df
#> # A tibble: 5 × 4
#>       x     y z1    z2   
#>   <dbl> <dbl> <chr> <chr>
#> 1     3     0 a     c    
#> 2     2     2 a     d    
#> 3     2     1 a     d    
#> 4     2     1 b     a    
#> 5     1     4 a     c    

# `pick()` provides a way to select a subset of your columns using
# tidyselect. It returns a data frame.
df |> mutate(cols = pick(x, y))
#> # A tibble: 5 × 5
#>       x     y z1    z2    cols$x    $y
#>   <dbl> <dbl> <chr> <chr>  <dbl> <dbl>
#> 1     3     0 a     c          3     0
#> 2     2     2 a     d          2     2
#> 3     2     1 a     d          2     1
#> 4     2     1 b     a          2     1
#> 5     1     4 a     c          1     4

# This is useful for functions that take data frames as inputs.
# For example, you can compute a joint rank between `x` and `y`.
df |> mutate(rank = dense_rank(pick(x, y)))
#> # A tibble: 5 × 5
#>       x     y z1    z2     rank
#>   <dbl> <dbl> <chr> <chr> <int>
#> 1     3     0 a     c         4
#> 2     2     2 a     d         3
#> 3     2     1 a     d         2
#> 4     2     1 b     a         2
#> 5     1     4 a     c         1

# `pick()` is also useful as a bridge between data-masking functions (like
# `mutate()` or `group_by()`) and functions with tidy-select behavior (like
# `select()`). For example, you can use `pick()` to create a wrapper around
# `group_by()` that takes a tidy-selection of columns to group on. For more
# bridge patterns, see
# https://rlang.r-lib.org/reference/topic-data-mask-programming.html#bridge-patterns.
my_group_by <- function(data, cols) {
  group_by(data, pick({{ cols }}))
}

df |> my_group_by(c(x, starts_with("z")))
#> # A tibble: 5 × 4
#> # Groups:   x, z1, z2 [4]
#>       x     y z1    z2   
#>   <dbl> <dbl> <chr> <chr>
#> 1     3     0 a     c    
#> 2     2     2 a     d    
#> 3     2     1 a     d    
#> 4     2     1 b     a    
#> 5     1     4 a     c    

# Or you can use it to dynamically select columns to `count()` by
df |> count(pick(starts_with("z")))
#> # A tibble: 3 × 3
#>   z1    z2        n
#>   <chr> <chr> <int>
#> 1 a     c         2
#> 2 a     d         2
#> 3 b     a         1
```
