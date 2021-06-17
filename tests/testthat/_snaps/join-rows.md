# join_rows() gives meaningful error message on incompatible types

    Code
      join_rows(data.frame(x = 1), data.frame(x = factor("a")))
    Error <rlang_error>
      Can't join on `x$x` x `y$x` because of incompatible types.
      i `x$x` is of type <double>>.
      i `y$x` is of type <factor<4d52a>>>.

# join_rows() gives meaningful error message on unmatched rows

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), complete = "x")
    Error <rlang_error>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), complete = "y")
    Error <rlang_error>
      Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), complete = "both")
    Error <rlang_error>
      Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), complete = "both")
    Error <rlang_error>
      Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

