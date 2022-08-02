# `na_matches` is validated

    Code
      join_mutate(df, df, by = "x", type = "left", na_matches = 1)
    Condition
      Error:
      ! `na_matches` must be a string or character vector.

---

    Code
      join_mutate(df, df, by = "x", type = "left", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".

---

    Code
      join_filter(df, df, by = "x", type = "semi", na_matches = 1)
    Condition
      Error:
      ! `na_matches` must be a string or character vector.

---

    Code
      join_filter(df, df, by = "x", type = "semi", na_matches = "foo")
    Condition
      Error:
      ! `na_matches` must be one of "na" or "never", not "foo".

# `unmatched` is validated

    Code
      join_mutate(df, df, by = "x", type = "left", unmatched = 1)
    Condition
      Error:
      ! `unmatched` must be a string or character vector.

---

    Code
      join_mutate(df, df, by = "x", type = "left", unmatched = "foo")
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "foo".

# `keep` is validated

    Code
      left_join(df, df, by = "x", keep = 1)
    Condition
      Error in `left_join()`:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`.

# mutating joins compute common columns

    Code
      out <- left_join(df1, df2)
    Message
      Joining, by = "x"

# filtering joins compute common columns

    Code
      out <- semi_join(df1, df2)
    Message
      Joining, by = "x"

# error if passed additional arguments

    Code
      inner_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      left_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      right_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      full_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      nest_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      anti_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"
    Code
      semi_join(df1, df2, on = "a")
    Message
      Joining, by = "a"
    Condition
      Error:
      ! Arguments in `...` must be used.
      x Problematic argument:
      * on = "a"

# nest_join computes common columns

    Code
      out <- nest_join(df1, df2)
    Message
      Joining, by = "x"

# validates inputs

    Code
      nest_join(df1, df2, by = 1)
    Condition
      Error in `as_join_by()`:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a double vector.
    Code
      nest_join(df1, df2, keep = 1)
    Condition
      Error in `nest_join()`:
      ! `keep` must be `TRUE`, `FALSE`, or `NULL`.
    Code
      nest_join(df1, df2, name = 1)
    Condition
      Error in `nest_join()`:
      ! `name` must be a string.
    Code
      nest_join(df1, df2, na_matches = 1)
    Condition
      Error in `nest_join()`:
      ! `na_matches` must be a string or character vector.
    Code
      nest_join(df1, df2, multiple = 1)
    Message
      Joining, by = "x"
    Condition
      Error in `vctrs::vec_locate_matches()`:
      ! `multiple` must be a string.
    Code
      nest_join(df1, df2, unmatched = 1)
    Condition
      Error in `nest_join()`:
      ! `unmatched` must be a string or character vector.

