# can't mix non-equi conditions with `keep = FALSE` (#6499)

    Code
      join_cols(c("x", "y"), c("x", "z"), by = join_by(x, y > z), keep = FALSE)
    Condition
      Error:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

---

    Code
      join_cols(c("xl", "xu"), c("yl", "yu"), by = join_by(xl >= yl, xu < yu), keep = FALSE)
    Condition
      Error:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

---

    Code
      join_cols("x", c("yl", "yu"), by = join_by(between(x, yl, yu)), keep = FALSE)
    Condition
      Error:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

---

    Code
      join_cols(c("xl", "xu"), c("yl", "yu"), by = join_by(overlaps(xl, xu, yl, yu)),
      keep = FALSE)
    Condition
      Error:
      ! Can't set `keep = FALSE` when using an inequality, rolling, or overlap join.

# can't duplicate key between equi condition and non-equi condition

    Code
      join_cols("x", c("xl", "xu"), by = join_by(x > xl, x == xu))
    Condition
      Error:
      ! Join columns in `x` must be unique.
      x Problem with `x`.

---

    Code
      join_cols(c("xl", "xu"), "x", by = join_by(xl < x, xu == x))
    Condition
      Error:
      ! Join columns in `y` must be unique.
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
      join_cols(xy, xy, by = as_join_by(list("1", y = "2")))
    Condition
      Error in `as_join_by()`:
      ! `by$x` must evaluate to a character vector.

---

    Code
      join_cols(xy, xy, by = as_join_by(list(x = "1", "2")))
    Condition
      Error in `as_join_by()`:
      ! `by$y` must evaluate to a character vector.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", NA)))
    Condition
      Error:
      ! Join columns in `x` can't be `NA`.
      x Problem at position 2.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("aaa", "bbb")))
    Condition
      Error:
      ! Join columns in `x` must be present in the data.
      x Problem with `aaa` and `bbb`.

---

    Code
      join_cols(xy, xy, by = as_join_by(c("x", "x", "x")))
    Condition
      Error:
      ! Join columns in `x` must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xyz, xyz, by = join_by(x, x > y, z))
    Condition
      Error:
      ! Join columns in `x` must be unique.
      x Problem with `x`.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = "x")
    Condition
      Error:
      ! `suffix` must be a character vector of length 2, not the string "x" of length 1.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = c("", NA))
    Condition
      Error:
      ! `suffix` can't be `NA`.

# references original column in `y` when there are type errors (#6465)

    Code
      (expect_error(join_cast_common(x_key, y_key, vars)))
    Output
      <error/dplyr_error_join_incompatible_type>
      Error:
      ! Can't join `x$a` with `y$b` due to incompatible types.
      i `x$a` is a <double>.
      i `y$b` is a <character>.

