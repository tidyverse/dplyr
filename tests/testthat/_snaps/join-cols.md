# emits useful messages

    Code
      join_cols(c("x", "y"), c("y", "y"), join_by(y))
    Error <rlang_error>
      Input columns in `y` must be unique.
      x Problem with `y`.

---

    Code
      join_cols(c("y", "y"), c("x", "y"), join_by(y))
    Error <rlang_error>
      Input columns in `x` must be unique.
      x Problem with `y`.

---

    Code
      standardise_join_by(NULL, xy, xy)
    Message <message>
      Joining, by = c("x", "y")
    Output
      Join By:
      - "x" == "x"
      - "y" == "y"

---

    Code
      standardise_join_by(NULL, xy, c("a", "b"))
    Error <rlang_error>
      `by` must be supplied when `x` and `y` have no common variables.
      i use by = character()` to perform a cross-join.

---

    Code
      standardise_join_by(FALSE, xy, xy)
    Error <rlang_error>
      `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a logical vector.

---

    Code
      join_cols(xy, xy, by = standardise_join_by(list(1, 2), xy, xy))
    Error <rlang_error>
      join columns must be character vectors.

---

    Code
      join_cols(xy, xy, by = standardise_join_by(c("x", "x"), xy, xy))
    Error <rlang_error>
      Join columns must be unique.
      x Problem at position 2.

---

    Code
      join_cols(xy, xy, by = standardise_join_by(c("x", NA), xy, xy))
    Error <rlang_error>
      Join columns must be not NA.
      x Problem at position 2.

---

    Code
      join_cols(xy, xy, by = standardise_join_by(c("aaa", "bbb"), xy, xy))
    Error <rlang_error>
      Join columns must be present in data.
      x Problem with `aaa` and `bbb`.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = "x")
    Error <rlang_error>
      `suffix` must be a character vector of length 2.
      i suffix is a character vector of length 1.

---

    Code
      join_cols(xy, xy, by = join_by(x), suffix = c("", NA))
    Error <rlang_error>
      `suffix` can't be NA.

