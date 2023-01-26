# can't use `keep = FALSE` with non-equi conditions (#6499)

    Code
      left_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
    Condition
      Error in `left_join()`:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

---

    Code
      full_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
    Condition
      Error in `full_join()`:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

# join_mutate() validates arguments

    Code
      join_mutate(df, df, by = 1, type = "left")
    Condition
      Error:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not the number 1.
    Code
      join_mutate(df, df, by = "x", type = "left", suffix = 1)
    Condition
      Error:
      ! `suffix` must be a character vector of length 2, not the number 1 of length 1.
    Code
      join_mutate(df, df, by = "x", type = "left", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".
    Code
      join_mutate(df, df, by = "x", type = "left", keep = 1)
    Condition
      Error:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`, not the number 1.

# join_filter() validates arguments

    Code
      join_filter(df, df, by = 1, type = "semi")
    Condition
      Error:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not the number 1.
    Code
      join_filter(df, df, by = "x", type = "semi", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".

# mutating joins trigger multiple match warning

    Code
      out <- left_join(df1, df2, join_by(x))
    Condition
      Warning in `left_join()`:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

# mutating joins compute common columns

    Code
      out <- left_join(df1, df2)
    Message
      Joining with `by = join_by(x)`

# filtering joins compute common columns

    Code
      out <- semi_join(df1, df2)
    Message
      Joining with `by = join_by(x)`

# mutating joins reference original column in `y` when there are type errors (#6465)

    Code
      (expect_error(left_join(x, y, by = join_by(a == b))))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error in `left_join()`:
      ! Can't join `x$a` with `y$b` due to incompatible types.
      i `x$a` is a <double>.
      i `y$b` is a <character>.

# filtering joins reference original column in `y` when there are type errors (#6465)

    Code
      (expect_error(semi_join(x, y, by = join_by(a == b))))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error in `semi_join()`:
      ! Can't join `x$a` with `y$b` due to incompatible types.
      i `x$a` is a <double>.
      i `y$b` is a <character>.

# error if passed additional arguments

    Code
      inner_join(df1, df2, on = "a")
    Condition
      Error in `inner_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      left_join(df1, df2, on = "a")
    Condition
      Error in `left_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      right_join(df1, df2, on = "a")
    Condition
      Error in `right_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      full_join(df1, df2, on = "a")
    Condition
      Error in `full_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      nest_join(df1, df2, on = "a")
    Condition
      Error in `nest_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      anti_join(df1, df2, on = "a")
    Condition
      Error in `anti_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"
    Code
      semi_join(df1, df2, on = "a")
    Condition
      Error in `semi_join()`:
      ! `...` must be empty.
      x Problematic argument:
      * on = "a"

# nest_join computes common columns

    Code
      out <- nest_join(df1, df2)
    Message
      Joining with `by = join_by(x)`

# nest_join references original column in `y` when there are type errors (#6465)

    Code
      (expect_error(nest_join(x, y, by = join_by(a == b))))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error in `nest_join()`:
      ! Can't join `x$a` with `y$b` due to incompatible types.
      i `x$a` is a <double>.
      i `y$b` is a <character>.

# validates inputs

    Code
      nest_join(df1, df2, by = 1)
    Condition
      Error in `nest_join()`:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not the number 1.
    Code
      nest_join(df1, df2, keep = 1)
    Condition
      Error in `nest_join()`:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`, not the number 1.
    Code
      nest_join(df1, df2, name = 1)
    Condition
      Error in `nest_join()`:
      ! `name` must be a single string, not the number 1.
    Code
      nest_join(df1, df2, na_matches = 1)
    Condition
      Error in `nest_join()`:
      ! `na_matches` must be a string or character vector.

# `by = character()` technically respects `unmatched`

    Code
      left_join(df1, df2, by = character(), unmatched = "error")
    Condition
      Error in `left_join()`:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

# `by = character()` technically respects `multiple`

    Code
      left_join(df, df, by = character(), multiple = "error")
    Condition
      Error in `left_join()`:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

# `by = character()` for a cross join is deprecated (#6604)

    Code
      out <- left_join(df1, df2, by = character())
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
      i Please use `cross_join()` instead.

---

    Code
      out <- semi_join(df1, df2, by = character())
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
      i Please use `cross_join()` instead.

---

    Code
      out <- nest_join(df1, df2, by = character())
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
      i Please use `cross_join()` instead.

# `by = named character()` for a cross join works

    Code
      out <- left_join(df1, df2, by = by)
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
      i Please use `cross_join()` instead.

# `by = list(x = character(), y = character())` for a cross join is deprecated (#6604)

    Code
      out <- left_join(df1, df2, by = list(x = character(), y = character()))
    Condition
      Warning:
      Using `by = character()` to perform a cross join was deprecated in dplyr 1.1.0.
      i Please use `cross_join()` instead.

