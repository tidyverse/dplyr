# Deprecated SE versions of main verbs.

**\[deprecated\]**

dplyr used to offer twin versions of each verb suffixed with an
underscore. These versions had standard evaluation (SE) semantics:
rather than taking arguments by code, like NSE verbs, they took
arguments by value. Their purpose was to make it possible to program
with dplyr. However, dplyr now uses tidy evaluation semantics. NSE verbs
still capture their arguments, but you can now unquote parts of these
arguments. This offers full programmability with NSE verbs. Thus, the
underscored versions are now superfluous.

Unquoting triggers immediate evaluation of its operand and inlines the
result within the captured expression. This result can be a value or an
expression to be evaluated later with the rest of the argument. See
[`vignette("programming")`](https://dplyr.tidyverse.org/dev/articles/programming.md)
for more information.

## Usage

``` r
add_count_(x, vars, wt = NULL, sort = FALSE)

add_tally_(x, wt, sort = FALSE)

arrange_(.data, ..., .dots = list())

count_(x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x))

distinct_(.data, ..., .dots, .keep_all = FALSE)

do_(.data, ..., .dots = list())

filter_(.data, ..., .dots = list())

funs_(dots, args = list(), env = base_env())

group_by_(.data, ..., .dots = list(), add = FALSE)

group_indices_(.data, ..., .dots = list())

mutate_(.data, ..., .dots = list())

tally_(x, wt, sort = FALSE)

transmute_(.data, ..., .dots = list())

rename_(.data, ..., .dots = list())

rename_vars_(vars, args)

select_(.data, ..., .dots = list())

select_vars_(vars, args, include = chr(), exclude = chr())

slice_(.data, ..., .dots = list())

summarise_(.data, ..., .dots = list())

summarize_(.data, ..., .dots = list())
```

## Arguments

- x:

  A [`tbl()`](https://dplyr.tidyverse.org/dev/reference/tbl.md)

- vars:

  Various meanings depending on the verb.

- wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Frequency weights. Can be `NULL` or a variable:

  - If `NULL` (the default), counts the number of rows in each group.

  - If a variable, computes `sum(wt)` for each group.

- sort:

  If `TRUE`, will show the largest groups at the top.

- .data:

  A data frame.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/dev/reference/group_by_drop_default.md)
  for details.

- .keep_all:

  If `TRUE`, keep all variables in `.data`. If a combination of `...` is
  not distinct, this keeps the first row of values.

- dots, .dots, ...:

  Pair/values of expressions coercible to lazy objects.

- args:

  Various meanings depending on the verb.

- env:

  The environment in which functions should be evaluated.

- add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

- include, exclude:

  Character vector of column names to always include/exclude.
