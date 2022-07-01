# takes the common type of `true` and `false` (#6243)

    Code
      if_else(TRUE, 1, "x")
    Condition
      Error in `if_else()`:
      ! Can't combine `true` <double> and `false` <character>.

# includes `missing` in the common type computation if used

    Code
      if_else(TRUE, 1, 2, missing = "x")
    Condition
      Error in `if_else()`:
      ! Can't combine `true` <double> and `missing` <character>.

# `condition` must be logical (and isn't cast to logical!)

    Code
      if_else(1:10, 1, 2)
    Condition
      Error in `if_else()`:
      ! `condition` must be a vector with type <logical>.
      Instead, it has type <integer>.

# `true`, `false`, and `missing` must recycle to the size of `condition`

    Code
      if_else(x < 2, bad, x)
    Condition
      Error in `if_else()`:
      ! `true` must have size 3, not size 2.

---

    Code
      if_else(x < 2, x, bad)
    Condition
      Error in `if_else()`:
      ! `false` must have size 3, not size 2.

---

    Code
      if_else(x < 2, x, x, missing = bad)
    Condition
      Error in `if_else()`:
      ! `missing` must have size 3, not size 2.

# must have empty dots

    Code
      if_else(TRUE, 1, 2, missing = 3, 4)
    Condition
      Error in `if_else()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 4
      i Did you forget to name an argument?

# `ptype` overrides the common type

    Code
      if_else(TRUE, 1L, 2.5, ptype = integer())
    Condition
      Error in `if_else()`:
      ! Can't convert from `false` <double> to <integer> due to loss of precision.
      * Locations: 1

# `size` overrides the `condition` size

    Code
      if_else(TRUE, 1, 2, size = 2)
    Condition
      Error in `if_else()`:
      ! `condition` must have size 2, not size 1.

