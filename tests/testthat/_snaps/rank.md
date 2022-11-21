# ntile() validates `n`

    Code
      ntile(1, n = 1.5)
    Condition
      Error in `ntile()`:
      ! `n` must be a whole number, not the number 1.5.

---

    Code
      ntile(1, n = c(1, 2))
    Condition
      Error in `ntile()`:
      ! `n` must be a whole number, not a double vector.

---

    Code
      ntile(1, n = NA_real_)
    Condition
      Error in `ntile()`:
      ! `n` must be a whole number, not a numeric `NA`.

---

    Code
      ntile(1, n = 0)
    Condition
      Error in `ntile()`:
      ! `n` must be positive.

