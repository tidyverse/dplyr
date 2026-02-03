# Select top (or bottom) n rows (by value)

**\[superseded\]** `top_n()` has been superseded in favour of
[`slice_min()`](https://dplyr.tidyverse.org/reference/slice.md)/[`slice_max()`](https://dplyr.tidyverse.org/reference/slice.md).
While it will not be deprecated in the near future, retirement means
that we will only perform critical bug fixes, so we recommend moving to
the newer alternatives.

`top_n()` was superseded because the name was fundamentally confusing as
it returned what you might reasonably consider to be the *bottom* rows.
Additionally, the `wt` variable had a confusing name, and strange
default (the last column in the data frame). Unfortunately we could not
see an easy way to fix the existing `top_n()` function without breaking
existing code, so we created a new alternative.

## Usage

``` r
top_n(x, n, wt)

top_frac(x, n, wt)
```

## Arguments

- x:

  A data frame.

- n:

  Number of rows to return for `top_n()`, fraction of rows to return for
  `top_frac()`. If `n` is positive, selects the top rows. If negative,
  selects the bottom rows. If `x` is grouped, this is the number (or
  fraction) of rows per group. Will include more rows if there are ties.

- wt:

  (Optional). The variable to use for ordering. If not specified,
  defaults to the last variable in the tbl.

## Examples

``` r
df <- data.frame(x = c(6, 4, 1, 10, 3, 1, 1))

df |> top_n(2)  # highest values
#> Selecting by x
#>    x
#> 1  6
#> 2 10
df |> top_n(-2) # lowest values
#> Selecting by x
#>   x
#> 1 1
#> 2 1
#> 3 1
# now use
df |> slice_max(x, n = 2)
#>    x
#> 1 10
#> 2  6
df |> slice_min(x, n = 2)
#>   x
#> 1 1
#> 2 1
#> 3 1

# top_frac() -> prop argument of slice_min()/slice_max()
df |> top_frac(.5)
#> Selecting by x
#>    x
#> 1  6
#> 2  4
#> 3 10
# ->
df |> slice_max(x, prop = 0.5)
#>    x
#> 1 10
#> 2  6
#> 3  4
```
