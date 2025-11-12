# Filtering joins

Filtering joins filter rows from `x` based on the presence or absence of
matches in `y`:

- `semi_join()` returns all rows from `x` with a match in `y`.

- `anti_join()` returns all rows from `x` with**out** a match in `y`.

## Usage

``` r
semi_join(x, y, by = NULL, copy = FALSE, ...)

# S3 method for class 'data.frame'
semi_join(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))

anti_join(x, y, by = NULL, copy = FALSE, ...)

# S3 method for class 'data.frame'
anti_join(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md),
  or a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/dev/reference/join_by.md)
  can also be used to perform inequality, rolling, and overlap joins.
  See the documentation at
  [?join_by](https://dplyr.tidyverse.org/dev/reference/join_by.md) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

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

## Value

An object of the same type as `x`. The output has the following
properties:

- Rows are a subset of the input, but appear in the same order.

- Columns are not modified.

- Data frame attributes are preserved.

- Groups are taken from `x`. The number of groups may be reduced.

## Methods

These function are **generic**s, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:

- `semi_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

- `anti_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

## See also

Other joins:
[`cross_join()`](https://dplyr.tidyverse.org/dev/reference/cross_join.md),
[`mutate-joins`](https://dplyr.tidyverse.org/dev/reference/mutate-joins.md),
[`nest_join()`](https://dplyr.tidyverse.org/dev/reference/nest_join.md)

## Examples

``` r
# "Filtering" joins keep cases from the LHS
band_members |> semi_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 2 × 2
#>   name  band   
#>   <chr> <chr>  
#> 1 John  Beatles
#> 2 Paul  Beatles
band_members |> anti_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 1 × 2
#>   name  band  
#>   <chr> <chr> 
#> 1 Mick  Stones

# To suppress the message about joining variables, supply `by`
band_members |> semi_join(band_instruments, by = join_by(name))
#> # A tibble: 2 × 2
#>   name  band   
#>   <chr> <chr>  
#> 1 John  Beatles
#> 2 Paul  Beatles
# This is good practice in production code
```
