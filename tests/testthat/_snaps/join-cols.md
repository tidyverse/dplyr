# can duplicate key between non-equi conditions

    Code
      join_cols("x", c("xl", "xu"), by = join_by(x > xl, x < xu), keep = FALSE)
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(c("xl", "xu"), "x", by = join_by(xl < x, xu > x), keep = FALSE)
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

# can't duplicate key between equi condition and non-equi condition

    Code
      join_cols("x", c("xl", "xu"), by = join_by(x > xl, x == xu))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(c("xl", "xu"), "x", by = join_by(xl < x, xu == x))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

# emits useful messages

    Code
      join_cols(c("x", "y"), c("y", "y"), join_by(y))
    Condition
      Error in `check_duplicate_vars()`:
      ! Input columns in `y` must be unique.
      x Problem with `y`.

---

    Code
      join_cols(c("y", "y"), c("x", "y"), join_by(y))
    Condition
      Error in `check_duplicate_vars()`:
      ! Input columns in `x` must be unique.
      x Problem with `y`.

---

    Code
      join_cols(xy, xy, by = as_join_by(list(1, 2)))
    Condition
      Error in `check_join_vars()`:
      ! join columns must be character vectors.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", NA)))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be not NA.
      x Problem at position 2.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("aaa", "bbb")))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be present in data.
      x Problem with `aaa` and `bbb`.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", "x", "x")))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xyz, xyz, by = join_by(x, x > y, z))
    Condition
      Error in `check_join_vars()`:
      ! Join columns must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = "x")
    Condition
      Error in `standardise_join_suffix()`:
      ! `suffix` must be a character vector of length 2.
      i suffix is a character vector of length 1.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = c("", NA))
    Condition
      Error in `glubort()`:
      ! `suffix` can't be NA.

