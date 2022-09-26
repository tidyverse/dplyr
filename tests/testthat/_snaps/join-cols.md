# can duplicate key between non-equi conditions

    Code
      join_cols("x", c("xl", "xu"), by = join_by(x > xl, x < xu), keep = FALSE)
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(c("xl", "xu"), "x", by = join_by(xl < x, xu > x), keep = FALSE)
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

# can't duplicate key between equi condition and non-equi condition

    Code
      join_cols("x", c("xl", "xu"), by = join_by(x > xl, x == xu))
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(c("xl", "xu"), "x", by = join_by(xl < x, xu == x))
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

# emits useful messages

    Code
      join_cols(c("x", "y"), c("y", "y"), join_by(y))
    Condition
      Error:
      ! Input columns in `y` must be unique.
      x Problem with `y`.

---

    Code
      join_cols(c("y", "y"), c("x", "y"), join_by(y))
    Condition
      Error:
      ! Input columns in `x` must be unique.
      x Problem with `y`.

---

    Code
      join_cols(xy, xy, by = as_join_by(list(1, 2)))
    Condition
      Error:
      ! Join columns must be character vectors.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", NA)))
    Condition
      Error:
      ! Join columns must be not NA.
      x Problem at position 2.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("aaa", "bbb")))
    Condition
      Error:
      ! Join columns must be present in data.
      x Problem with `aaa` and `bbb`.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", "x", "x")))
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xyz, xyz, by = join_by(x, x > y, z))
    Condition
      Error:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = "x")
    Condition
      Error:
      ! `suffix` must be a character vector of length 2, not a string of length 1.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = c("", NA))
    Condition
      Error:
      ! `suffix` can't be NA.

# references original column in `y` when there are type errors (#6465)

    Code
      (expect_error(join_cast_common(x_key, y_key, vars)))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error:
      ! Can't join `x$a` with `y$b` due to incompatible types.
      i `x$a` is a <double>.
      i `y$b` is a <character>.

