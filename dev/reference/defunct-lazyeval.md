# Defunct standard evaluation functions

**\[defunct\]**

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

select_(.data, ..., .dots = list())

slice_(.data, ..., .dots = list())

summarise_(.data, ..., .dots = list())

summarize_(.data, ..., .dots = list())
```
