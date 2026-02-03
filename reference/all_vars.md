# Apply predicate to all variables

**\[superseded\]**

`all_vars()` and `any_vars()` were only needed for the scoped verbs,
which have been superseded by the use of
[`across()`](https://dplyr.tidyverse.org/reference/across.md) in an
existing verb. See
[`vignette("colwise")`](https://dplyr.tidyverse.org/articles/colwise.md)
for details.

These quoting functions signal to scoped filtering verbs (e.g.
[`filter_if()`](https://dplyr.tidyverse.org/reference/filter_all.md) or
[`filter_all()`](https://dplyr.tidyverse.org/reference/filter_all.md))
that a predicate expression should be applied to all relevant variables.
The `all_vars()` variant takes the intersection of the predicate
expressions with `&` while the `any_vars()` variant takes the union with
`|`.

## Usage

``` r
all_vars(expr)

any_vars(expr)
```

## Arguments

- expr:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  An expression that returns a logical vector, using `.` to refer to the
  "current" variable.

## See also

[`vars()`](https://dplyr.tidyverse.org/reference/vars.md) for other
quoting functions that you can use with scoped verbs.
