# join_rows() gives meaningful error message on incompatible types

    Code
      join_rows(data.frame(x = 1), data.frame(x = factor("a")))
    Error <rlang_error>
      Can't join on `x$x` x `y$x` because of incompatible types.
      i `x$x` is of type <double>>.
      i `y$x` is of type <factor<4d52a>>>.

# join_rows() gives meaningful error/warning message on multiple matches

    Code
      join_rows(1, c(1, 1), multiple = "error")
    Error <rlang_error>
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
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), check_unmatched = "x")
    Error <rlang_error>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), check_unmatched = "y")
    Error <rlang_error>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), check_unmatched = "both")
    Error <rlang_error>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), check_unmatched = "both")
    Error <rlang_error>
      Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() gives meaningful error message on duplicated keys

    Code
      join_rows(data.frame(x = c(1, 1)), data.frame(x = c(3, 1)), check_duplicates = "x")
    Error <rlang_error>
      `x` must not contain duplicate keys.
      i Row 2 is a duplicate.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1, 3)),
      check_duplicates = "y")
    Error <rlang_error>
      `y` must not contain duplicate keys.
      i Row 3 is a duplicate.

---

    Code
      join_rows(data.frame(x = c(1, 1)), data.frame(x = 1), check_duplicates = "both")
    Error <rlang_error>
      `x` must not contain duplicate keys.
      i Row 2 is a duplicate.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 1)), check_duplicates = "both")
    Error <rlang_error>
      `y` must not contain duplicate keys.
      i Row 2 is a duplicate.

# join_rows() gives meaningful error message on missing `x` keys

    Code
      join_rows(c(1, 2, 3, NA), c(1, 2, NA, 3), na_matches = "error")
    Error <rlang_error>
      The keys of `x` must not contain missing values.
      i Row 4 contains a missing value.

