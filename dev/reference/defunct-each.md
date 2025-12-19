# Defunct functions for working with multiple columns

**\[defunct\]**

`mutate_each()` and `summarise_each()` are deprecated in favour of the
new [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md)
function that works within
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
and [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md).

## Usage

``` r
summarise_each(tbl, funs, ...)

summarise_each_(tbl, funs, vars)

mutate_each(tbl, funs, ...)

mutate_each_(tbl, funs, vars)

summarize_each(tbl, funs, ...)

summarize_each_(tbl, funs, vars)
```
