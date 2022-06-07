# odd numbered inputs can be size zero

    Code
      vec_case_when(logical(), 1:2)
    Condition
      Error:
      ! `..2` must have size 0, not size 2.

# even numbered inputs are cast to their common type

    Code
      vec_case_when(FALSE, 1, TRUE, "x")
    Condition
      Error:
      ! Can't combine `..2` <double> and `..4` <character>.

# even numbered inputs must be size 1 or same size as logical conditions

    Code
      vec_case_when(c(TRUE, FALSE, TRUE, TRUE), 1:3)
    Condition
      Error:
      ! `..2` must have size 4, not size 3.

# `.default` must be size 1 or same size as logical conditions (exact same as any other even numbered input)

    Code
      vec_case_when(FALSE, 1L, .default = 2:3)
    Condition
      Error:
      ! `.default` must have size 1, not size 2.

# `.missing` must be size 1 or same size as logical conditions (exact same as any other even numbered input)

    Code
      vec_case_when(FALSE, 1L, .missing = 2:3)
    Condition
      Error:
      ! `.missing` must have size 1, not size 2.

# `.default_arg` can be customized

    Code
      vec_case_when(FALSE, 1L, .default = 2:3, .default_arg = "foo")
    Condition
      Error:
      ! `foo` must have size 1, not size 2.

---

    Code
      vec_case_when(FALSE, 1L, .default = "x", .default_arg = "foo")
    Condition
      Error:
      ! Can't combine `..2` <integer> and `foo` <character>.

# `.missing_arg` can be customized

    Code
      vec_case_when(FALSE, 1L, .missing = 2:3, .missing_arg = "foo")
    Condition
      Error:
      ! `foo` must have size 1, not size 2.

---

    Code
      vec_case_when(FALSE, 1L, .missing = "x", .missing_arg = "foo")
    Condition
      Error:
      ! Can't combine `..2` <integer> and `foo` <character>.

# `.default_arg` is validated

    Code
      vec_case_when(TRUE, 1, .default_arg = 1)
    Condition
      Error:
      ! `.default_arg` must be a string.

# `.missing_arg` is validated

    Code
      vec_case_when(TRUE, 1, .missing_arg = 1)
    Condition
      Error:
      ! `.missing_arg` must be a string.

# odd numbered inputs must all be the same size

    Code
      vec_case_when(c(TRUE, FALSE), 1, TRUE, 2)
    Condition
      Error:
      ! `..3` must have size 2, not size 1.

---

    Code
      vec_case_when(c(TRUE, FALSE), 1, c(TRUE, FALSE, TRUE), 2)
    Condition
      Error:
      ! Can't recycle `..1` (size 2) to match `..3` (size 3).

# odd numbered inputs must be logical (and aren't cast to logical!)

    Code
      vec_case_when(1, 2)
    Condition
      Error:
      ! `..1` must be a vector with type <logical>.
      Instead, it has type <double>.

---

    Code
      vec_case_when(TRUE, 2, 3.5, 4)
    Condition
      Error:
      ! `..3` must be a vector with type <logical>.
      Instead, it has type <double>.

# `.size` overrides the odd numbered input sizes

    Code
      vec_case_when(TRUE, 1, .size = 5)
    Condition
      Error:
      ! `..1` must have size 5, not size 1.

---

    Code
      vec_case_when(c(TRUE, FALSE), 1, c(TRUE, FALSE, TRUE), 2, .size = 2)
    Condition
      Error:
      ! `..3` must have size 2, not size 3.

# `.ptype` overrides the even numbered input types

    Code
      vec_case_when(FALSE, 1, TRUE, 2, .ptype = character())
    Condition
      Error:
      ! Can't convert `..2` <double> to <character>.

# can't have an odd number of inputs

    Code
      vec_case_when(1)
    Condition
      Error:
      ! `...` must contain an even number of inputs.
      i 1 inputs were provided.

---

    Code
      vec_case_when(1, 2, 3)
    Condition
      Error:
      ! `...` must contain an even number of inputs.
      i 3 inputs were provided.

# can't have empty dots

    Code
      vec_case_when()
    Condition
      Error:
      ! `...` can't be empty.

---

    Code
      vec_case_when(.default = 1)
    Condition
      Error:
      ! `...` can't be empty.

# named dots show up in the error message

    Code
      vec_case_when(x = 1.5, 1)
    Condition
      Error:
      ! `x` must be a vector with type <logical>.
      Instead, it has type <double>.

---

    Code
      vec_case_when(x = TRUE, 1, y = c(TRUE, FALSE), 2)
    Condition
      Error:
      ! `x` must have size 2, not size 1.

---

    Code
      vec_case_when(TRUE, 1, FALSE, x = "y")
    Condition
      Error:
      ! Can't combine `..2` <double> and `x` <character>.

