# Select variables

**\[superseded\]**

`vars()` is superseded because it is only needed for the scoped verbs
(i.e.
[`mutate_at()`](https://dplyr.tidyverse.org/reference/mutate_all.md),
[`summarise_at()`](https://dplyr.tidyverse.org/reference/summarise_all.md),
and friends), which have been been superseded in favour of
[`across()`](https://dplyr.tidyverse.org/reference/across.md). See
[`vignette("colwise")`](https://dplyr.tidyverse.org/articles/colwise.md)
for details.

This helper is intended to provide tidy-select semantics for scoped
verbs like
[`mutate_at()`](https://dplyr.tidyverse.org/reference/mutate_all.md) and
[`summarise_at()`](https://dplyr.tidyverse.org/reference/summarise_all.md).
Note that anywhere you can supply `vars()` specification, you can also
supply a numeric vector of column positions or a character vector of
column names.

## Usage

``` r
vars(...)
```

## Arguments

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.md)\>
  Variables to operate on.

## See also

[`all_vars()`](https://dplyr.tidyverse.org/reference/all_vars.md) and
[`any_vars()`](https://dplyr.tidyverse.org/reference/all_vars.md) for
other quoting functions that you can use with scoped verbs.
