# Combine values from multiple columns

`c_across()` is designed to work with
[`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md) to
make it easy to perform row-wise aggregations. It has two differences
from [`c()`](https://rdrr.io/r/base/c.html):

- It uses tidy select semantics so you can easily select multiple
  variables. See
  [`vignette("rowwise")`](https://dplyr.tidyverse.org/dev/articles/rowwise.md)
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

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Columns to transform. You can't select grouping columns because they
  are already automatically handled by the verb (i.e.
  [`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
  or [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md)).

## See also

[`across()`](https://dplyr.tidyverse.org/dev/reference/across.md) for a
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
#>      id     w      x     y     z   sum    sd
#>   <int> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1 0.126 0.533  0.172 0.196  1.03 0.186
#> 2     2 0.938 0.547  0.691 0.969  3.14 0.202
#> 3     3 0.801 0.0959 0.675 0.387  1.96 0.315
#> 4     4 0.758 0.388  0.946 0.650  2.74 0.233
```
