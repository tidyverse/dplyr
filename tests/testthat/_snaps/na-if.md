# is type stable on `x`

    Code
      na_if(0L, 1.5)
    Condition
      Error in `na_if()`:
      ! Can't convert from `y` <double> to `x` <integer> due to loss of precision.
      * Locations: 1

# is size stable on `x`

    Code
      na_if(1, integer())
    Condition
      Error in `na_if()`:
      ! Can't recycle `y` (size 0) to size 1.

---

    Code
      na_if(1, c(1, 2))
    Condition
      Error in `na_if()`:
      ! Can't recycle `y` (size 2) to size 1.

---

    Code
      na_if(c(1, 2, 3), c(1, 2))
    Condition
      Error in `na_if()`:
      ! Can't recycle `y` (size 2) to size 3.

# requires vector types for `x` and `y`

    Code
      na_if(environment(), 1)
    Condition
      Error in `na_if()`:
      ! `x` must be a vector, not an environment.

---

    Code
      na_if(1, environment())
    Condition
      Error in `na_if()`:
      ! `y` must be a vector, not an environment.

