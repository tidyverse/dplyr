# Elementwise `any()` and `all()`

These functions are variants of
[`any()`](https://rdrr.io/r/base/any.html) and
[`all()`](https://rdrr.io/r/base/all.html) that work elementwise across
multiple inputs. You can also think of these functions as generalizing
[`|`](https://rdrr.io/r/base/Logic.html) and `&` to any number of
inputs, rather than just two, for example:

- `when_any(x, y, z)` is equivalent to `x | y | z`.

- `when_all(x, y, z)` is equivalent to `x & y & z`.

`when_any()` is particularly useful within
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md) and
[`filter_out()`](https://dplyr.tidyverse.org/dev/reference/filter.md) to
specify comma separated conditions combined with `|` rather than `&`.

## Usage

``` r
when_any(..., na_rm = FALSE, size = NULL)

when_all(..., na_rm = FALSE, size = NULL)
```

## Arguments

- ...:

  Logical vectors of equal size.

- na_rm:

  Missing value handling:

  - If `FALSE`, missing values are propagated according to the same
    rules as `|` and `&`.

  - If `TRUE`, missing values are removed from the elementwise
    computation.

- size:

  An optional output size. Only useful to specify if it is possible for
  `...` to be empty, with no inputs provided.

## Details

`when_any()` and `when_all()` are "parallel" versions of
[`any()`](https://rdrr.io/r/base/any.html) and
[`all()`](https://rdrr.io/r/base/all.html) in the same way that
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html) are "parallel" versions
of [`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html).

## See also

[`base::any()`](https://rdrr.io/r/base/any.html),
[`base::all()`](https://rdrr.io/r/base/all.html),
[`cumany()`](https://dplyr.tidyverse.org/dev/reference/cumall.md),
[`cumall()`](https://dplyr.tidyverse.org/dev/reference/cumall.md),
[`base::pmin()`](https://rdrr.io/r/base/Extremes.html),
[`base::pmax()`](https://rdrr.io/r/base/Extremes.html)

## Examples

``` r
x <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
y <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)

# `any()` and `all()` summarise down to 1 value
any(x, y)
#> [1] TRUE
all(x, y)
#> [1] FALSE

# `when_any()` and `when_all()` work element by element across all inputs
# at the same time. Their defaults are equivalent to calling `|` or `&`.
when_any(x, y)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE    NA  TRUE    NA    NA
x | y
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE    NA  TRUE    NA    NA

when_all(x, y)
#> [1]  TRUE FALSE    NA FALSE FALSE FALSE    NA FALSE    NA
x & y
#> [1]  TRUE FALSE    NA FALSE FALSE FALSE    NA FALSE    NA

# `na_rm = TRUE` is useful when you'd like to force these functions to
# return only `TRUE` or `FALSE`. This argument does so by removing any `NA`
# from the elementwise computation entirely.
tibble(
  x = x,
  y = y,
  any_propagate = when_any(x, y),
  any_remove = when_any(x, y, na_rm = TRUE),
  all_propagate = when_all(x, y),
  all_remove = when_all(x, y, na_rm = TRUE)
)
#> # A tibble: 9 × 6
#>   x     y     any_propagate any_remove all_propagate all_remove
#>   <lgl> <lgl> <lgl>         <lgl>      <lgl>         <lgl>     
#> 1 TRUE  TRUE  TRUE          TRUE       TRUE          TRUE      
#> 2 TRUE  FALSE TRUE          TRUE       FALSE         FALSE     
#> 3 TRUE  NA    TRUE          TRUE       NA            TRUE      
#> 4 FALSE TRUE  TRUE          TRUE       FALSE         FALSE     
#> 5 FALSE FALSE FALSE         FALSE      FALSE         FALSE     
#> 6 FALSE NA    NA            FALSE      FALSE         FALSE     
#> 7 NA    TRUE  TRUE          TRUE       NA            TRUE      
#> 8 NA    FALSE NA            FALSE      FALSE         FALSE     
#> 9 NA    NA    NA            FALSE      NA            TRUE      

# ---------------------------------------------------------------------------
# With `filter()` and `filter_out()`

# `when_any()` is particularly useful inside of `filter()` and
# `filter_out()` as a way to combine comma separated conditions with `|`
# instead of with `&`.

countries <- tibble(
  name = c("US", "CA", "PR", "RU", "US", NA, "CA", "PR", "RU"),
  score = c(200, 100, 150, NA, 50, 100, 300, 250, 120)
)
countries
#> # A tibble: 9 × 2
#>   name  score
#>   <chr> <dbl>
#> 1 US      200
#> 2 CA      100
#> 3 PR      150
#> 4 RU       NA
#> 5 US       50
#> 6 NA      100
#> 7 CA      300
#> 8 PR      250
#> 9 RU      120

# Find rows where any of the following are true:
# - "US" and "CA" have a score between 200-300
# - "PR" and "RU" have a score between 100-200
countries |>
  filter(
    (name %in% c("US", "CA") & between(score, 200, 300)) |
      (name %in% c("PR", "RU") & between(score, 100, 200))
  )
#> # A tibble: 4 × 2
#>   name  score
#>   <chr> <dbl>
#> 1 US      200
#> 2 PR      150
#> 3 CA      300
#> 4 RU      120

# With `when_any()`, you drop the explicit `|`, the extra `()`, and your
# conditions are all indented to the same level
countries |>
  filter(when_any(
    name %in% c("US", "CA") & between(score, 200, 300),
    name %in% c("PR", "RU") & between(score, 100, 200)
  ))
#> # A tibble: 4 × 2
#>   name  score
#>   <chr> <dbl>
#> 1 US      200
#> 2 PR      150
#> 3 CA      300
#> 4 RU      120

# To drop these rows instead, use `filter_out()`
countries |>
  filter_out(when_any(
    name %in% c("US", "CA") & between(score, 200, 300),
    name %in% c("PR", "RU") & between(score, 100, 200)
  ))
#> # A tibble: 5 × 2
#>   name  score
#>   <chr> <dbl>
#> 1 CA      100
#> 2 RU       NA
#> 3 US       50
#> 4 NA      100
#> 5 PR      250

# ---------------------------------------------------------------------------
# Programming with `when_any()` and `when_all()`

# The `size` argument is useful for making these functions size stable when
# you aren't sure how many inputs you're going to receive
size <- length(x)

# Two inputs
inputs <- list(x, y)
when_all(!!!inputs, size = size)
#> [1]  TRUE FALSE    NA FALSE FALSE FALSE    NA FALSE    NA

# One input
inputs <- list(x)
when_all(!!!inputs, size = size)
#> [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE    NA    NA    NA

# Zero inputs (without `size`, this would return `logical()`)
inputs <- list()
when_all(!!!inputs, size = size)
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# When no inputs are provided, these functions are consistent with `any()`
# and `all()`
any()
#> [1] FALSE
when_any(size = 1)
#> [1] FALSE

all()
#> [1] TRUE
when_all(size = 1)
#> [1] TRUE
```
