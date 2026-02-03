# Mutating joins

Mutating joins add columns from `y` to `x`, matching observations based
on the keys. There are four mutating joins: the inner join, and the
three outer joins.

### Inner join

An `inner_join()` only keeps observations from `x` that have a matching
key in `y`.

The most important property of an inner join is that unmatched rows in
either input are not included in the result. This means that generally
inner joins are not appropriate in most analyses, because it is too easy
to lose observations.

### Outer joins

The three outer joins keep observations that appear in at least one of
the data frames:

- A `left_join()` keeps all observations in `x`.

- A `right_join()` keeps all observations in `y`.

- A `full_join()` keeps all observations in `x` and `y`.

## Usage

``` r
inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'data.frame'
inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
)

left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'data.frame'
left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
)

right_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'data.frame'
right_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
)

full_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'data.frame'
full_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  relationship = NULL
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

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  Other parameters passed onto methods.

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

  - If `NULL`, the default, joins on equality retain only the keys from
    `x`, while joins on inequality retain the keys from both inputs.

  - If `TRUE`, all keys from both inputs are retained.

  - If `FALSE`, only keys from `x` are retained. For right and full
    joins, the data in key columns corresponding to rows that only exist
    in `y` are merged into the key columns from `x`. Can't be used when
    joining on inequality conditions.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://rdrr.io/r/base/merge.html).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

- multiple:

  Handling of rows in `x` with multiple matches in `y`. For each row of
  `x`:

  - `"all"`, the default, returns every match detected in `y`. This is
    the same behavior as SQL.

  - `"any"` returns one match detected in `y`, with no guarantees on
    which match will be returned. It is often faster than `"first"` and
    `"last"` if you just need to detect if there is at least one match.

  - `"first"` returns the first match detected in `y`.

  - `"last"` returns the last match detected in `y`.

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

- relationship:

  Handling of the expected relationship between the keys of `x` and `y`.
  If the expectations chosen from the list below are invalidated, an
  error is thrown.

  - `NULL`, the default, doesn't expect there to be any relationship
    between `x` and `y`. However, for equality joins it will check for a
    many-to-many relationship (which is typically unexpected) and will
    warn if one occurs, encouraging you to either take a closer look at
    your inputs or make this relationship explicit by specifying
    `"many-to-many"`.

    See the *Many-to-many relationships* section for more details.

  - `"one-to-one"` expects:

    - Each row in `x` matches at most 1 row in `y`.

    - Each row in `y` matches at most 1 row in `x`.

  - `"one-to-many"` expects:

    - Each row in `y` matches at most 1 row in `x`.

  - `"many-to-one"` expects:

    - Each row in `x` matches at most 1 row in `y`.

  - `"many-to-many"` doesn't perform any relationship checks, but is
    provided to allow you to be explicit about this relationship if you
    know it exists.

  `relationship` doesn't handle cases where there are zero matches. For
  that, see `unmatched`.

## Value

An object of the same type as `x` (including the same groups). The order
of the rows and columns of `x` is preserved as much as possible. The
output has the following properties:

- The rows are affect by the join type.

  - `inner_join()` returns matched `x` rows.

  - `left_join()` returns all `x` rows.

  - `right_join()` returns matched of `x` rows, followed by unmatched
    `y` rows.

  - `full_join()` returns all `x` rows, followed by unmatched `y` rows.

- Output columns include all columns from `x` and all non-key columns
  from `y`. If `keep = TRUE`, the key columns from `y` are included as
  well.

- If non-key columns in `x` and `y` have the same name, `suffix`es are
  added to disambiguate. If `keep = TRUE` and key columns in `x` and `y`
  have the same name, `suffix`es are added to disambiguate these as
  well.

- If `keep = FALSE`, output columns included in `by` are coerced to
  their common type between `x` and `y`.

## Many-to-many relationships

By default, dplyr guards against many-to-many relationships in equality
joins by throwing a warning. These occur when both of the following are
true:

- A row in `x` matches multiple rows in `y`.

- A row in `y` matches multiple rows in `x`.

This is typically surprising, as most joins involve a relationship of
one-to-one, one-to-many, or many-to-one, and is often the result of an
improperly specified join. Many-to-many relationships are particularly
problematic because they can result in a Cartesian explosion of the
number of rows returned from the join.

If a many-to-many relationship is expected, silence this warning by
explicitly setting `relationship = "many-to-many"`.

In production code, it is best to preemptively set `relationship` to
whatever relationship you expect to exist between the keys of `x` and
`y`, as this forces an error to occur immediately if the data doesn't
align with your expectations.

Inequality joins typically result in many-to-many relationships by
nature, so they don't warn on them by default, but you should still take
extra care when specifying an inequality join, because they also have
the capability to return a large number of rows.

Rolling joins don't warn on many-to-many relationships either, but many
rolling joins follow a many-to-one relationship, so it is often useful
to set `relationship = "many-to-one"` to enforce this.

Note that in SQL, most database providers won't let you specify a
many-to-many relationship between two tables, instead requiring that you
create a third *junction table* that results in two one-to-many
relationships instead.

## Methods

These functions are **generic**s, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:

- `inner_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

- `left_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

- `right_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

- `full_join()`: dbplyr
  ([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/join.tbl_sql.html)),
  dplyr (`data.frame`) .

## See also

Other joins:
[`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.md),
[`filter-joins`](https://dplyr.tidyverse.org/reference/filter-joins.md),
[`nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.md)

## Examples

``` r
band_members |> inner_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 2 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Paul  Beatles bass  
band_members |> left_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 3 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  NA    
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass  
band_members |> right_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 3 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Paul  Beatles bass  
#> 3 Keith NA      guitar
band_members |> full_join(band_instruments)
#> Joining with `by = join_by(name)`
#> # A tibble: 4 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  NA    
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass  
#> 4 Keith NA      guitar

# To suppress the message about joining variables, supply `by`
band_members |> inner_join(band_instruments, by = join_by(name))
#> # A tibble: 2 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 John  Beatles guitar
#> 2 Paul  Beatles bass  
# This is good practice in production code

# Use an equality expression if the join variables have different names
band_members |> full_join(band_instruments2, by = join_by(name == artist))
#> # A tibble: 4 × 3
#>   name  band    plays 
#>   <chr> <chr>   <chr> 
#> 1 Mick  Stones  NA    
#> 2 John  Beatles guitar
#> 3 Paul  Beatles bass  
#> 4 Keith NA      guitar
# By default, the join keys from `x` and `y` are coalesced in the output; use
# `keep = TRUE` to keep the join keys from both `x` and `y`
band_members |>
  full_join(band_instruments2, by = join_by(name == artist), keep = TRUE)
#> # A tibble: 4 × 4
#>   name  band    artist plays 
#>   <chr> <chr>   <chr>  <chr> 
#> 1 Mick  Stones  NA     NA    
#> 2 John  Beatles John   guitar
#> 3 Paul  Beatles Paul   bass  
#> 4 NA    NA      Keith  guitar

# If a row in `x` matches multiple rows in `y`, all the rows in `y` will be
# returned once for each matching row in `x`.
df1 <- tibble(x = 1:3)
df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
df1 |> left_join(df2)
#> Joining with `by = join_by(x)`
#> # A tibble: 4 × 2
#>       x y     
#>   <dbl> <chr> 
#> 1     1 first 
#> 2     1 second
#> 3     2 third 
#> 4     3 NA    

# If a row in `y` also matches multiple rows in `x`, this is known as a
# many-to-many relationship, which is typically a result of an improperly
# specified join or some kind of messy data. In this case, a warning is
# thrown by default:
df3 <- tibble(x = c(1, 1, 1, 3))
df3 |> left_join(df2)
#> Joining with `by = join_by(x)`
#> Warning: Detected an unexpected many-to-many relationship between `x` and `y`.
#> ℹ Row 1 of `x` matches multiple rows in `y`.
#> ℹ Row 1 of `y` matches multiple rows in `x`.
#> ℹ If a many-to-many relationship is expected, set `relationship =
#>   "many-to-many"` to silence this warning.
#> # A tibble: 7 × 2
#>       x y     
#>   <dbl> <chr> 
#> 1     1 first 
#> 2     1 second
#> 3     1 first 
#> 4     1 second
#> 5     1 first 
#> 6     1 second
#> 7     3 NA    

# In the rare case where a many-to-many relationship is expected, set
# `relationship = "many-to-many"` to silence this warning
df3 |> left_join(df2, relationship = "many-to-many")
#> Joining with `by = join_by(x)`
#> # A tibble: 7 × 2
#>       x y     
#>   <dbl> <chr> 
#> 1     1 first 
#> 2     1 second
#> 3     1 first 
#> 4     1 second
#> 5     1 first 
#> 6     1 second
#> 7     3 NA    

# Use `join_by()` with a condition other than `==` to perform an inequality
# join. Here we match on every instance where `df1$x > df2$x`.
df1 |> left_join(df2, join_by(x > x))
#> # A tibble: 6 × 3
#>     x.x   x.y y     
#>   <int> <dbl> <chr> 
#> 1     1    NA NA    
#> 2     2     1 first 
#> 3     2     1 second
#> 4     3     1 first 
#> 5     3     1 second
#> 6     3     2 third 

# By default, NAs match other NAs so that there are two
# rows in the output of this join:
df1 <- data.frame(x = c(1, NA), y = 2)
df2 <- data.frame(x = c(1, NA), z = 3)
left_join(df1, df2)
#> Joining with `by = join_by(x)`
#>    x y z
#> 1  1 2 3
#> 2 NA 2 3

# You can optionally request that NAs don't match, giving a
# a result that more closely resembles SQL joins
left_join(df1, df2, na_matches = "never")
#> Joining with `by = join_by(x)`
#>    x y  z
#> 1  1 2  3
#> 2 NA 2 NA
```
