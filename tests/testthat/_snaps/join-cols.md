# emits useful messages

    Input columns in `y` must be unique.
    x Problem with `y`.

---

    Input columns in `x` must be unique.
    x Problem with `y`.

---

    Code
      vars <- join_cols(xy, xy)
    Message <message>
      Joining, by = c("x", "y")

---

    `by` must be supplied when `x` and `y` have no common variables.
    i use by = character()` to perform a cross-join.

---

    `by` must be a (named) character vector, list, or NULL, not a logical vector.

---

    join columns must be character vectors.

---

    Join columns must be unique.
    x Problem at position 2.

---

    Join columns must be not NA.
    x Problem at position 2.

---

    Join columns must be present in data.
    x Problem with `aaa` and `bbb`.

---

    `suffix` must be a character vector of length 2.
    i suffix is a character vector of length 1.

---

    `suffix` can't be NA.

