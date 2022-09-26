# takes the common type between all inputs (#6478)

    Code
      between("1", 2, 3)
    Condition
      Error in `between()`:
      ! Can't combine `x` <character> and `left` <double>.

---

    Code
      between(1, "2", 3)
    Condition
      Error in `between()`:
      ! Can't combine `x` <double> and `left` <character>.

---

    Code
      between(1, 2, "3")
    Condition
      Error in `between()`:
      ! Can't combine `x` <double> and `right` <character>.

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

