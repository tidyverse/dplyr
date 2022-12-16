# `multiple` default behavior is correct

    Code
      out <- join_rows(c(1, 1), c(1, 1), condition = "==")
    Condition
      Warning:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

---

    Code
      out <- join_rows(c(1, 1), c(1, 1), condition = ">=", filter = "max")
    Condition
      Warning:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

# join_rows() allows `unmatched` to be specified independently for inner joins

    Code
      join_rows(c(1, 3), c(1, 2), type = "inner", unmatched = c("drop", "error"))
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() expects incompatible type errors to have been handled by join_cast_common()

    Code
      (expect_error(join_rows(data.frame(x = 1), data.frame(x = factor("a")))))
    Output
      <error/rlang_error>
      Error:
      ! `join_cast_common()` should have handled this.
      i This is an internal error that was detected in the dplyr package.
        Please report it at <https://github.com/tidyverse/dplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.

# join_rows() gives meaningful error/warning message on multiple matches

    Code
      join_rows(1, c(1, 1), multiple = "error")
    Condition
      Error:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

---

    Code
      cat(conditionMessage(cnd))
    Output
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

---

    Code
      . <- left_join(df1, df2, by = "x")
    Condition
      Warning in `left_join()`:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

# join_rows() gives meaningful error message on unmatched rows

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "left",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "nest",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "right",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), type = "inner",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), type = "inner",
      unmatched = c("error", "drop"))
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = c("drop", "error"))
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() always errors on unmatched missing values

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = 1), type = "right", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "right", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, NA)), type = "inner",
      unmatched = "error", na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, NA)), type = "inner",
      unmatched = c("drop", "error"), na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = "error", na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = c("error", "drop"), na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "inner", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

# join_rows() validates `unmatched`

    Code
      join_rows(df, df, unmatched = 1)
    Condition
      Error:
      ! `unmatched` must be a character vector, not a number.
    Code
      join_rows(df, df, unmatched = "foo")
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "foo".
    Code
      join_rows(df, df, type = "left", unmatched = character())
    Condition
      Error:
      ! `unmatched` must be length 1, not 0.
    Code
      join_rows(df, df, type = "left", unmatched = c("drop", "error"))
    Condition
      Error:
      ! `unmatched` must be length 1, not 2.
    Code
      join_rows(df, df, type = "inner", unmatched = character())
    Condition
      Error:
      ! `unmatched` must be length 1 or 2, not 0.
    Code
      join_rows(df, df, type = "inner", unmatched = c("drop", "error", "error"))
    Condition
      Error:
      ! `unmatched` must be length 1 or 2, not 3.
    Code
      join_rows(df, df, type = "inner", unmatched = c("drop", "dr"))
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "dr".
      i Did you mean "drop"?

