# ntile() validates `n`

    Code
      ntile(1, n = 1.5)
    Condition
      Error in `ntile()`:
      ! `n` must be a round number, not a number.

---

    Code
      ntile(1, n = c(1, 2))
    Condition
      Error in `ntile()`:
      ! `n` must be a round number, not a double vector.

---

    Code
      ntile(1, n = NA_real_)
    Condition
      Error in `ntile()`:
      ! `n` must be a round number, not a numeric `NA`.

---

    Code
      ntile(1, n = 0)
    Condition
      Error in `ntile()`:
      ! `n` must be positive.

