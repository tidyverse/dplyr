# casts `left` and `right` to the type of `x`

    Code
      between(1L, 1.5, 2L)
    Condition
      Error in `between()`:
      ! Can't convert from `left` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      between(1L, 1L, 2.5)
    Condition
      Error in `between()`:
      ! Can't convert from `right` <double> to <integer> due to loss of precision.
      * Locations: 1

# recycles `left` and `right` to the size of `x`

    Code
      between(1:3, 1:2, 1L)
    Condition
      Error in `between()`:
      ! Can't recycle `left` (size 2) to size 3.

---

    Code
      between(1:3, 1L, 1:2)
    Condition
      Error in `between()`:
      ! Can't recycle `right` (size 2) to size 3.

