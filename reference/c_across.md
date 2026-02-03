# Combine values from multiple columns

`c_across()` is designed to work with
[`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.md) to make
it easy to perform row-wise aggregations. It has two differences from
[`c()`](https://rdrr.io/r/base/c.html):

- It uses tidy select semantics so you can easily select multiple
  variables. See
  [`vignette("rowwise")`](https://dplyr.tidyverse.org/articles/rowwise.md)
  for more details.

- It uses
  [`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html) in
  order to give safer outputs.

## Usage

``` r
c_across(cols)
```

## Arguments

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Columns to transform. You can't select grouping columns because they
  are already automatically handled by the verb (i.e.
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.md) or
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.md)).

## See also

[`across()`](https://dplyr.tidyverse.org/reference/across.md) for a
function that returns a tibble.

## Examples

``` r
df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
df |>
  rowwise() |>
  mutate(
    sum = sum(c_across(w:z)),
    sd = sd(c_across(w:z))
  )
#> # A tibble: 4 × 7
#> # Rowwise: 
#>      id      w      x     y      z   sum    sd
#>   <int>  <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1     1 0.815  0.435  0.970 0.836   3.06 0.230
#> 2     2 0.0710 0.552  0.179 0.605   1.41 0.267
#> 3     3 0.527  0.204  0.778 0.907   2.42 0.310
#> 4     4 0.763  0.0310 0.886 0.0359  1.72 0.459
```
