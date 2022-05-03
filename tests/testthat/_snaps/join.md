# `na_matches` is validated

    Code
      join_mutate(df, df, by = "x", type = "left", na_matches = 1)
    Condition
      Error in `arg_match0()`:
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
      Error in `arg_match0()`:
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
      Error in `arg_match0()`:
      ! `unmatched` must be a string or character vector.

---

    Code
      join_mutate(df, df, by = "x", type = "left", unmatched = "foo")
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "foo".

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

# nest_join computes common columns

    Code
      out <- nest_join(df1, df2)
    Message
      Joining, by = "x"

