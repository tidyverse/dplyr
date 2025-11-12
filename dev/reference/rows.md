# Manipulate individual rows

These functions provide a framework for modifying rows in a table using
a second table of data. The two tables are matched `by` a set of key
variables whose values typically uniquely identify each row. The
functions are inspired by SQL's `INSERT`, `UPDATE`, and `DELETE`, and
can optionally modify `in_place` for selected backends.

- `rows_insert()` adds new rows (like `INSERT`). By default, key values
  in `y` must not exist in `x`.

- `rows_append()` works like `rows_insert()` but ignores keys.

- `rows_update()` modifies existing rows (like `UPDATE`). Key values in
  `y` must be unique, and, by default, key values in `y` must exist in
  `x`.

- `rows_patch()` works like `rows_update()` but only overwrites `NA`
  values.

- `rows_upsert()` inserts or updates depending on whether or not the key
  value in `y` already exists in `x`. Key values in `y` must be unique.

- `rows_delete()` deletes rows (like `DELETE`). By default, key values
  in `y` must exist in `x`.

## Usage

``` r
rows_insert(
  x,
  y,
  by = NULL,
  ...,
  conflict = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

rows_append(x, y, ..., copy = FALSE, in_place = FALSE)

rows_update(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

rows_patch(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

rows_upsert(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

rows_delete(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)
```

## Arguments

- x, y:

  A pair of data frames or data frame extensions (e.g. a tibble). `y`
  must have the same columns of `x` or a subset.

- by:

  An unnamed character vector giving the key columns. The key columns
  must exist in both `x` and `y`. Keys typically uniquely identify each
  row, but this is only enforced for the key values of `y` when
  `rows_update()`, `rows_patch()`, or `rows_upsert()` are used.

  By default, we use the first column in `y`, since the first column is
  a reasonable place to put an identifier variable.

- ...:

  Other parameters passed onto methods.

- conflict:

  For `rows_insert()`, how should keys in `y` that conflict with keys in
  `x` be handled? A conflict arises if there is a key in `y` that
  already exists in `x`.

  One of:

  - `"error"`, the default, will error if there are any keys in `y` that
    conflict with keys in `x`.

  - `"ignore"` will ignore rows in `y` with keys that conflict with keys
    in `x`.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- in_place:

  Should `x` be modified in place? This argument is only relevant for
  mutable backends (e.g. databases, data.tables).

  When `TRUE`, a modified version of `x` is returned invisibly; when
  `FALSE`, a new object representing the resulting changes is returned.

- unmatched:

  For `rows_update()`, `rows_patch()`, and `rows_delete()`, how should
  keys in `y` that are unmatched by the keys in `x` be handled?

  One of:

  - `"error"`, the default, will error if there are any keys in `y` that
    are unmatched by the keys in `x`.

  - `"ignore"` will ignore rows in `y` with keys that are unmatched by
    the keys in `x`.

## Value

An object of the same type as `x`. The order of the rows and columns of
`x` is preserved as much as possible. The output has the following
properties:

- `rows_update()` and `rows_patch()` preserve the number of rows;
  `rows_insert()`, `rows_append()`, and `rows_upsert()` return all
  existing rows and potentially new rows; `rows_delete()` returns a
  subset of the rows.

- Columns are not added, removed, or relocated, though the data may be
  updated.

- Groups are taken from `x`.

- Data frame attributes are taken from `x`.

If `in_place = TRUE`, the result will be returned invisibly.

## Methods

These function are **generic**s, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages:

- `rows_insert()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

- `rows_append()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

- `rows_update()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

- `rows_patch()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

- `rows_upsert()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

- `rows_delete()`: dbplyr (`tbl_lazy`), dplyr (`data.frame`) .

## Examples

``` r
data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
data
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 NA      2.5

# Insert
rows_insert(data, tibble(a = 4, b = "z"))
#> Matching, by = "a"
#> # A tibble: 4 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 NA      2.5
#> 4     4 z      NA  

# By default, if a key in `y` matches a key in `x`, then it can't be inserted
# and will throw an error. Alternatively, you can ignore rows in `y`
# containing keys that conflict with keys in `x` with `conflict = "ignore"`,
# or you can use `rows_append()` to ignore keys entirely.
try(rows_insert(data, tibble(a = 3, b = "z")))
#> Matching, by = "a"
#> Error in rows_insert(data, tibble(a = 3, b = "z")) : 
#>   `y` can't contain keys that already exist in `x`.
#> ℹ The following rows in `y` have keys that already exist in `x`:
#>   `c(1)`.
#> ℹ Use `conflict = "ignore"` if you want to ignore these `y` rows.
rows_insert(data, tibble(a = 3, b = "z"), conflict = "ignore")
#> Matching, by = "a"
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 NA      2.5
rows_append(data, tibble(a = 3, b = "z"))
#> # A tibble: 4 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 NA      2.5
#> 4     3 z      NA  

# Update
rows_update(data, tibble(a = 2:3, b = "z"))
#> Matching, by = "a"
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 z       1.5
#> 3     3 z       2.5
rows_update(data, tibble(b = "z", a = 2:3), by = "a")
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 z       1.5
#> 3     3 z       2.5

# Variants: patch and upsert
rows_patch(data, tibble(a = 2:3, b = "z"))
#> Matching, by = "a"
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 z       2.5
rows_upsert(data, tibble(a = 2:4, b = "z"))
#> Matching, by = "a"
#> # A tibble: 4 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 z       1.5
#> 3     3 z       2.5
#> 4     4 z      NA  

# Delete and truncate
rows_delete(data, tibble(a = 2:3))
#> Matching, by = "a"
#> # A tibble: 1 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
rows_delete(data, tibble(a = 2:3, b = "b"))
#> Matching, by = "a"
#> Ignoring extra `y` columns: b
#> # A tibble: 1 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5

# By default, for update, patch, and delete it is an error if a key in `y`
# doesn't exist in `x`. You can ignore rows in `y` that have unmatched keys
# with `unmatched = "ignore"`.
y <- tibble(a = 3:4, b = "z")
try(rows_update(data, y, by = "a"))
#> Error in rows_update(data, y, by = "a") : 
#>   `y` must contain keys that already exist in `x`.
#> ℹ The following rows in `y` have keys that don't exist in `x`: `c(2)`.
#> ℹ Use `unmatched = "ignore"` if you want to ignore these `y` rows.
rows_update(data, y, by = "a", unmatched = "ignore")
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 z       2.5
rows_patch(data, y, by = "a", unmatched = "ignore")
#> # A tibble: 3 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
#> 3     3 z       2.5
rows_delete(data, y, by = "a", unmatched = "ignore")
#> Ignoring extra `y` columns: b
#> # A tibble: 2 × 3
#>       a b         c
#>   <int> <chr> <dbl>
#> 1     1 a       0.5
#> 2     2 b       1.5
```
