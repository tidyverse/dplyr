# `conditions` inputs can be size zero

    Code
      vec_case_when(list(logical()), list(1:2))
    Condition
      Error in `vec_case_when()`:
      ! `values[[1]]` must have size 0, not size 2.

# `values` are cast to their common type

    Code
      vec_case_when(list(FALSE, TRUE), list(1, "x"))
    Condition
      Error in `vec_case_when()`:
      ! Can't combine `values[[1]]` <double> and `values[[2]]` <character>.

# `values` must be size 1 or same size as the `conditions`

    Code
      vec_case_when(list(c(TRUE, FALSE, TRUE, TRUE)), list(1:3))
    Condition
      Error in `vec_case_when()`:
      ! `values[[1]]` must have size 4, not size 3.

# `default` must be size 1 or same size as `conditions` (exact same as any other `values` input)

    Code
      vec_case_when(list(FALSE), list(1L), default = 2:3)
    Condition
      Error in `vec_case_when()`:
      ! `default` must have size 1, not size 2.

# `default_arg` can be customized

    Code
      vec_case_when(list(FALSE), list(1L), default = 2:3, default_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! `foo` must have size 1, not size 2.

---

    Code
      vec_case_when(list(FALSE), list(1L), default = "x", default_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! Can't combine `values[[1]]` <integer> and `foo` <character>.

# `conditions_arg` is validated

    Code
      vec_case_when(list(TRUE), list(1), conditions_arg = 1)
    Condition
      Error in `vec_case_when()`:
      ! `conditions_arg` must be a string.

# `values_arg` is validated

    Code
      vec_case_when(list(TRUE), list(1), values_arg = 1)
    Condition
      Error in `vec_case_when()`:
      ! `values_arg` must be a string.

# `default_arg` is validated

    Code
      vec_case_when(list(TRUE), list(1), default_arg = 1)
    Condition
      Error in `vec_case_when()`:
      ! `default_arg` must be a string.

# `conditions` must all be the same size

    Code
      vec_case_when(list(c(TRUE, FALSE), TRUE), list(1, 2))
    Condition
      Error in `vec_case_when()`:
      ! `conditions[[2]]` must have size 2, not size 1.

---

    Code
      vec_case_when(list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)), list(1, 2))
    Condition
      Error in `vec_case_when()`:
      ! Can't recycle `conditions[[1]]` (size 2) to match `conditions[[2]]` (size 3).

# `conditions` must be logical (and aren't cast to logical!)

    Code
      vec_case_when(list(1), list(2))
    Condition
      Error in `vec_case_when()`:
      ! `conditions[[1]]` must be a logical vector, not the number 1.

---

    Code
      vec_case_when(list(TRUE, 3.5), list(2, 4))
    Condition
      Error in `vec_case_when()`:
      ! `conditions[[2]]` must be a logical vector, not the number 3.5.

# `size` overrides the `conditions` sizes

    Code
      vec_case_when(list(TRUE), list(1), size = 5)
    Condition
      Error in `vec_case_when()`:
      ! `conditions[[1]]` must have size 5, not size 1.

---

    Code
      vec_case_when(list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)), list(1, 2), size = 2)
    Condition
      Error in `vec_case_when()`:
      ! `conditions[[2]]` must have size 2, not size 3.

# `ptype` overrides the `values` types

    Code
      vec_case_when(list(FALSE, TRUE), list(1, 2), ptype = character())
    Condition
      Error in `vec_case_when()`:
      ! Can't convert `values[[1]]` <double> to <character>.

# number of `conditions` and `values` must be the same

    Code
      vec_case_when(list(TRUE), list())
    Condition
      Error in `vec_case_when()`:
      ! The number of supplied conditions (1) must equal the number of supplied values (0).

---

    Code
      vec_case_when(list(TRUE, TRUE), list(1))
    Condition
      Error in `vec_case_when()`:
      ! The number of supplied conditions (2) must equal the number of supplied values (1).

# can't have empty inputs

    Code
      vec_case_when(list(), list())
    Condition
      Error in `vec_case_when()`:
      ! At least one condition must be supplied.

---

    Code
      vec_case_when(list(), list(), default = 1)
    Condition
      Error in `vec_case_when()`:
      ! At least one condition must be supplied.

# dots must be empty

    Code
      vec_case_when(list(TRUE), list(1), 2)
    Condition
      Error in `vec_case_when()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

# `conditions` must be a list

    Code
      vec_case_when(1, list(2))
    Condition
      Error in `vec_case_when()`:
      ! `conditions` must be a list, not the number 1.

# `values` must be a list

    Code
      vec_case_when(list(TRUE), 1)
    Condition
      Error in `vec_case_when()`:
      ! `values` must be a list, not the number 1.

# named inputs show up in the error message

    Code
      vec_case_when(list(x = 1.5), list(1))
    Condition
      Error in `vec_case_when()`:
      ! `conditions$x` must be a logical vector, not the number 1.5.

---

    Code
      vec_case_when(list(x = 1.5), list(1), conditions_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! `foo$x` must be a logical vector, not the number 1.5.

---

    Code
      vec_case_when(list(x = 1.5), list(1), conditions_arg = "")
    Condition
      Error in `vec_case_when()`:
      ! `x` must be a logical vector, not the number 1.5.

---

    Code
      vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2))
    Condition
      Error in `vec_case_when()`:
      ! `conditions$x` must have size 2, not size 1.

---

    Code
      vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2), conditions_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! `foo$x` must have size 2, not size 1.

---

    Code
      vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2), conditions_arg = "")
    Condition
      Error in `vec_case_when()`:
      ! `x` must have size 2, not size 1.

---

    Code
      vec_case_when(list(TRUE, FALSE), list(1, x = "y"))
    Condition
      Error in `vec_case_when()`:
      ! Can't combine `values[[1]]` <double> and `values$x` <character>.

---

    Code
      vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! Can't combine `foo[[1]]` <double> and `foo$x` <character>.

---

    Code
      vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "")
    Condition
      Error in `vec_case_when()`:
      ! Can't combine `..1` <double> and `x` <character>.

---

    Code
      vec_case_when(list(TRUE), list(NULL))
    Condition
      Error in `vec_case_when()`:
      ! `values[[1]]` must be a vector, not `NULL`.

---

    Code
      vec_case_when(list(TRUE), list(x = NULL))
    Condition
      Error in `vec_case_when()`:
      ! `values$x` must be a vector, not `NULL`.

---

    Code
      vec_case_when(list(TRUE), list(NULL), values_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! `foo[[1]]` must be a vector, not `NULL`.

---

    Code
      vec_case_when(list(TRUE), list(x = NULL), values_arg = "foo")
    Condition
      Error in `vec_case_when()`:
      ! `foo$x` must be a vector, not `NULL`.

---

    Code
      vec_case_when(list(TRUE), list(NULL), values_arg = "")
    Condition
      Error in `vec_case_when()`:
      ! `..1` must be a vector, not `NULL`.

---

    Code
      vec_case_when(list(TRUE), list(x = NULL), values_arg = "")
    Condition
      Error in `vec_case_when()`:
      ! `x` must be a vector, not `NULL`.

