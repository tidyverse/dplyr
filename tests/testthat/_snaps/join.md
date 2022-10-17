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
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a number.
    Code
      join_mutate(df, df, by = "x", type = "left", suffix = 1)
    Condition
      Error:
      ! `suffix` must be a character vector of length 2, not a number of length 1.
    Code
      join_mutate(df, df, by = "x", type = "left", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".
    Code
      join_mutate(df, df, by = "x", type = "left", unmatched = "foo")
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "foo".
    Code
      join_mutate(df, df, by = "x", type = "left", keep = 1)
    Condition
      Error:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`, not a number.

# join_filter() validates arguments

    Code
      join_filter(df, df, by = 1, type = "semi")
    Condition
      Error:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a number.
    Code
      join_filter(df, df, by = "x", type = "semi", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".

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
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `inner_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      left_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `left_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      right_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `right_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      full_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `full_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      nest_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `nest_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      anti_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `anti_join()`:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      semi_join(df1, df2, on = "a")
    Message
      Joining with `by = join_by(a)`
    Condition
      Error in `semi_join()`:
      ! Arguments in `...` must be used.
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
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a number.
    Code
      nest_join(df1, df2, keep = 1)
    Condition
      Error in `nest_join()`:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`, not a number.
    Code
      nest_join(df1, df2, name = 1)
    Condition
      Error in `nest_join()`:
      ! `name` must be a single string, not a number.
    Code
      nest_join(df1, df2, na_matches = 1)
    Condition
      Error in `nest_join()`:
      ! `na_matches` must be a string or character vector.
    Code
      nest_join(df1, df2, unmatched = 1)
    Condition
      Error in `nest_join()`:
      ! `unmatched` must be a string or character vector.

