# `multiple` default behavior is correct

    Code
      out <- join_rows(c(1, 1), c(1, 1), condition = "==")
    Warning <dplyr_warning_join_matches_multiple>
      Each row in `x` can match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

---

    Code
      out <- join_rows(c(1, 1), c(1, 1), condition = ">=", filter = "max")
    Warning <dplyr_warning_join_matches_multiple>
      Each row in `x` can match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

# join_rows() gives meaningful error message on incompatible types

    Code
      join_rows(data.frame(x = 1), data.frame(x = factor("a")))
    Error <dplyr_error_join_incompatible_type>
      Can't join `x$x` with `y$x` because of incompatible types.
      i `x$x` is of type <double>.
      i `y$x` is of type <factor<4d52a>>.

# join_rows() gives meaningful error/warning message on multiple matches

    Code
      join_rows(1, c(1, 1), multiple = "error")
    Error <dplyr_error_join_matches_multiple>
      Each row in `x` can match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

---

    Code
      cat(cnd$message)
    Output
      Each row in `x` can match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.

# join_rows() gives meaningful error message on unmatched rows

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "left",
      unmatched = "error")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "nest",
      unmatched = "error")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "right",
      unmatched = "error")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), type = "inner",
      unmatched = "error")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = "error")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() always errors on unmatched missing values

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "na")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "never")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "na")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "never")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = 1), type = "right", unmatched = "error",
      na_matches = "na")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "right", unmatched = "error",
      na_matches = "never")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, NA)), type = "inner",
      unmatched = "error", na_matches = "na")
    Error <dplyr_error_join_matches_remaining>
      Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = "error", na_matches = "na")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "inner", unmatched = "error",
      na_matches = "never")
    Error <dplyr_error_join_matches_nothing>
      Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

