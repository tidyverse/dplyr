# `lead()` / `lag()` catch negative `n`

    Code
      lead(1:5, -1)
    Condition
      Error in `lead()`:
      ! `n` must be positive.

---

    Code
      lag(1:5, -1)
    Condition
      Error in `lag()`:
      ! `n` must be positive.

# `lead()` / `lag()` check `n` properties before checking if positive

    Code
      lead(1:5, n = 1:2)
    Condition
      Error in `lead()`:
      ! `n` must have size 1, not size 2.

---

    Code
      lag(1:5, n = 1:2)
    Condition
      Error in `lag()`:
      ! `n` must have size 1, not size 2.

---

    Code
      lead(1:5, n = "x")
    Condition
      Error in `lead()`:
      ! Can't convert `n` <character> to <integer>.

---

    Code
      lag(1:5, n = "x")
    Condition
      Error in `lag()`:
      ! Can't convert `n` <character> to <integer>.

---

    Code
      lead(1:5, n = NA_integer_)
    Condition
      Error in `lead()`:
      ! `n` can't be missing.

---

    Code
      lag(1:5, n = NA_integer_)
    Condition
      Error in `lag()`:
      ! `n` can't be missing.

# `lead()` / `lag()` check for empty dots

    Code
      lead(1:5, deault = 1)
    Condition
      Error in `lead()`:
      ! `...` must be empty.
      x Problematic argument:
      * deault = 1

---

    Code
      lag(1:5, deault = 1)
    Condition
      Error in `lag()`:
      ! `...` must be empty.
      x Problematic argument:
      * deault = 1

# `lag()` errors on <ts> objects

    Code
      lag(ts(1:10))
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not a <ts>, do you want `stats::lag()`?

# `lead()` / `lag()` require that `x` is a vector

    Code
      lead(environment())
    Condition
      Error in `lead()`:
      ! `x` must be a vector, not an environment.

---

    Code
      lag(environment())
    Condition
      Error in `lag()`:
      ! `x` must be a vector, not an environment.

# `default` is cast to the type of `x` (#6330)

    Code
      shift(1L, default = 1.5)
    Condition
      Error:
      ! Can't convert from `default` <double> to `x` <integer> due to loss of precision.
      * Locations: 1

# `default` must be size 1 (#5641)

    Code
      shift(1:5, default = 1:2)
    Condition
      Error:
      ! `default` must have size 1, not size 2.

---

    Code
      shift(1:5, default = integer())
    Condition
      Error:
      ! `default` must have size 1, not size 0.

# `n` is validated

    Code
      shift(1, n = 1:2)
    Condition
      Error:
      ! `n` must have size 1, not size 2.

---

    Code
      shift(1, n = "x")
    Condition
      Error:
      ! Can't convert `n` <character> to <integer>.

---

    Code
      shift(1, n = NA_integer_)
    Condition
      Error:
      ! `n` can't be missing.

# `order_by` must be the same size as `x`

    Code
      shift(1:5, order_by = 1:4)
    Condition
      Error in `with_order()`:
      ! `order_by` must have size 5, not size 4.

