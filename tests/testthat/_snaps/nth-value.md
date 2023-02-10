# `na_rm` is validated

    Code
      nth(1, 1, na_rm = 1)
    Condition
      Error in `nth()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      nth(1, 1, na_rm = c(TRUE, FALSE))
    Condition
      Error in `nth()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not a logical vector.

# `default` must be size 1 (when not used with lists)

    Code
      nth(1L, n = 2L, default = 1:2)
    Condition
      Error in `nth()`:
      ! `default` must have size 1, not size 2.

# `default` is cast to the type of `x` (when not used with lists)

    Code
      nth("x", 2, default = 2)
    Condition
      Error in `nth()`:
      ! Can't convert `default` <double> to match type of `x` <character>.

# `n` is validated (#5466)

    Code
      nth(1:10, n = "x")
    Condition
      Error in `nth()`:
      ! Can't convert `n` <character> to <integer>.

---

    Code
      nth(1:10, n = 1:2)
    Condition
      Error in `nth()`:
      ! `n` must have size 1, not size 2.

---

    Code
      nth(1:10, n = NA_integer_)
    Condition
      Error in `nth()`:
      ! `n` can't be `NA`.

# `x` must be a vector

    Code
      nth(environment(), 1L)
    Condition
      Error in `vec_size()`:
      ! `x` must be a vector, not an environment.

# `order_by` must be the same size as `x`

    Code
      nth(1:5, n = 1L, order_by = 1:2)
    Condition
      Error in `nth()`:
      ! `order_by` must have size 5, not size 2.

---

    Code
      nth(1:5, n = 6L, order_by = 1:2)
    Condition
      Error in `nth()`:
      ! `order_by` must have size 5, not size 2.

