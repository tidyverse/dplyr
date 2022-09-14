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

# join_rows() gives meaningful error message on incompatible types

    Code
      (expect_error(join_rows(data.frame(x = 1), data.frame(x = factor("a")))))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error:
      ! Can't join `x$x` with `y$x` because of incompatible types.
      i `x$x` is of type <double>.
      i `y$x` is of type <factor<4d52a>>.

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
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = "error")
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
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = "error", na_matches = "na")
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

