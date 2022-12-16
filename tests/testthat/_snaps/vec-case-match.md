# `haystacks` must be castable to `needles`

    Code
      vec_case_match(1L, haystacks = list(1.5), values = list(2))
    Condition
      Error in `vec_case_match()`:
      ! Can't convert from `haystacks[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

# `ptype` overrides `values` common type

    Code
      vec_case_match(1:2, haystacks = list(1), values = list(1.5), ptype = integer())
    Condition
      Error in `vec_case_match()`:
      ! Can't convert from `values[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

# `default` respects `ptype`

    Code
      vec_case_match(needles = 1, haystacks = list(1), values = list(2L), default = 1.5,
      ptype = integer())
    Condition
      Error in `vec_case_match()`:
      ! Can't convert from `default` <double> to <integer> due to loss of precision.
      * Locations: 1

# `NULL` values in `haystacks` and `values` are not dropped

    Code
      vec_case_match(1:2, list(1, NULL, 2), list("a", NULL, "b"))
    Condition
      Error in `vec_case_match()`:
      ! `haystacks[[2]]` must be a vector, not `NULL`.

---

    Code
      vec_case_match(1:2, list(1, NULL, 2), list("a", "a", "b"))
    Condition
      Error in `vec_case_match()`:
      ! `haystacks[[2]]` must be a vector, not `NULL`.

---

    Code
      vec_case_match(1:2, list(1, 1, 2), list("a", NULL, "b"))
    Condition
      Error in `vec_case_match()`:
      ! `values[[2]]` must be a vector, not `NULL`.

# size of `needles` is maintained

    Code
      vec_case_match(1, haystacks = list(1), values = list(1:2))
    Condition
      Error in `vec_case_match()`:
      ! `values[[1]]` must have size 1, not size 2.

# requires at least one condition

    Code
      vec_case_match(1, haystacks = list(), values = list())
    Condition
      Error in `vec_case_match()`:
      ! At least one condition must be supplied.

# input must be a vector

    Code
      vec_case_match(environment(), haystacks = list(environment()), values = list(1))
    Condition
      Error in `vec_case_match()`:
      ! `needles` must be a vector, not an environment.

# `haystacks` must be a list

    Code
      vec_case_match(1, haystacks = 1, values = list(2))
    Condition
      Error in `vec_case_match()`:
      ! `haystacks` must be a list, not a number.

# `values` must be a list

    Code
      vec_case_match(1, haystacks = list(1), values = 2)
    Condition
      Error in `vec_case_match()`:
      ! `values` must be a list, not a number.

# `needles_arg` is respected

    Code
      vec_case_match(needles = environment(), haystacks = list(environment()),
      values = list(1), needles_arg = "foo")
    Condition
      Error in `vec_case_match()`:
      ! `foo` must be a vector, not an environment.

---

    Code
      vec_case_match(needles = environment(), haystacks = list(environment()),
      values = list(1), needles_arg = "")
    Condition
      Error in `vec_case_match()`:
      ! Input must be a vector, not an environment.

# `haystacks_arg` is respected

    Code
      vec_case_match(needles = 1, haystacks = 1, values = list(1), haystacks_arg = "foo")
    Condition
      Error in `vec_case_match()`:
      ! `foo` must be a list, not a number.

---

    Code
      vec_case_match(needles = 1, haystacks = 1, values = list(1), haystacks_arg = "")
    Condition
      Error in `vec_case_match()`:
      ! Input must be a list, not a number.

---

    Code
      vec_case_match(needles = 1, haystacks = list(a = "x"), values = list(1),
      haystacks_arg = "foo")
    Condition
      Error in `vec_case_match()`:
      ! Can't convert `foo$a` <character> to <double>.

---

    Code
      vec_case_match(needles = 1, haystacks = list("x"), values = list(1),
      haystacks_arg = "")
    Condition
      Error in `vec_case_match()`:
      ! Can't convert `..1` <character> to <double>.

# `values_arg` is respected

    Code
      vec_case_match(needles = 1, haystacks = list(1, 2), values = list("x", b = 1),
      values_arg = "foo")
    Condition
      Error in `vec_case_match()`:
      ! Can't combine `foo[[1]]` <character> and `foo$b` <double>.

---

    Code
      vec_case_match(needles = 1, haystacks = list(1, 2), values = list("x", b = 1),
      values_arg = "")
    Condition
      Error in `vec_case_match()`:
      ! Can't combine `..1` <character> and `b` <double>.

# `default_arg` is respected

    Code
      vec_case_match(needles = 1, haystacks = list(1), values = list(2L), default = 1.5,
      default_arg = "foo", ptype = integer())
    Condition
      Error in `vec_case_match()`:
      ! Can't convert from `foo` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_case_match(needles = 1, haystacks = list(1), values = list(2L), default = 1.5,
      default_arg = "", ptype = integer())
    Condition
      Error in `vec_case_match()`:
      ! Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1

