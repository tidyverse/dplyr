# Nest join

A nest join leaves `x` almost unchanged, except that it adds a new
list-column, where each element contains the rows from `y` that match
the corresponding row in `x`.

## Usage

``` r
nest_join(x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ...)

# S3 method for class 'data.frame'
nest_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = NULL,
  name = NULL,
  ...,
  na_matches = c("na", "never"),
  unmatched = "drop"
)
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.md), or a
  character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.md)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.md)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.md) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.md) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.md).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- keep:

  Should the new list-column contain join keys? The default will
  preserve the join keys for inequality joins.

- name:

  The name of the list-column created by the join. If `NULL`, the
  default, the name of `y` is used.

- ...:

  Other parameters passed onto methods.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://rdrr.io/r/base/merge.html).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

- unmatched:

  How should unmatched keys that would result in dropped rows be
  handled?

  - `"drop"` drops unmatched keys from the result.

  - `"error"` throws an error if unmatched keys are detected.

  `unmatched` is intended to protect you from accidentally dropping rows
  during a join. It only checks for unmatched keys in the input that
  could potentially drop rows.

  - For left joins, it checks `y`.

  - For right joins, it checks `x`.

  - For inner joins, it checks both `x` and `y`. In this case,
    `unmatched` is also allowed to be a character vector of length 2 to
    specify the behavior for `x` and `y` independently.

## Value

The output:

- Is same type as `x` (including having the same groups).

- Has exactly the same number of rows as `x`.

- Contains all the columns of `x` in the same order with the same
  values. They are only modified (slightly) if `keep = FALSE`, when
  columns listed in `by` will be coerced to their common type across `x`
  and `y`.

- Gains one new column called `{name}` on the far right, a list column
  containing data frames the same type as `y`.

## Relationship to other joins

You can recreate many other joins from the result of a nest join:

- [`inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.md)
  is a `nest_join()` plus
  [`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html).

- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.md)
  is a `nest_join()` plus `tidyr::unnest(keep_empty = TRUE)`.

- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.md)
  is a `nest_join()` plus a
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.md) where
  you check that every element of data has at least one row.

- [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.md)
  is a `nest_join()` plus a
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.md) where
  you check that every element has zero rows.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dplyr
(`data.frame`) .

## See also

Other joins:
[`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.md),
[`filter-joins`](https://dplyr.tidyverse.org/reference/filter-joins.md),
[`mutate-joins`](https://dplyr.tidyverse.org/reference/mutate-joins.md)

## Examples

``` r
df1 <- tibble(x = 1:3)
df2 <- tibble(x = c(2, 3, 3), y = c("a", "b", "c"))

out <- nest_join(df1, df2)
#> Joining with `by = join_by(x)`
out
#> # A tibble: 3 × 2
#>       x df2             
#>   <dbl> <list>          
#> 1     1 <tibble [0 × 1]>
#> 2     2 <tibble [1 × 1]>
#> 3     3 <tibble [2 × 1]>
out$df2
#> [[1]]
#> # A tibble: 0 × 1
#> # ℹ 1 variable: y <chr>
#> 
#> [[2]]
#> # A tibble: 1 × 1
#>   y    
#>   <chr>
#> 1 a    
#> 
#> [[3]]
#> # A tibble: 2 × 1
#>   y    
#>   <chr>
#> 1 b    
#> 2 c    
#> 
```
