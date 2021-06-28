# slice_min/max() check size of `order_by=` (#5922)

    Code
      slice_min(data.frame(x = 1:10), 1:6)
    Error <dplyr_error>
      `slice_min(order_by=)` must have size 10, not size 6.
    Code
      slice_max(data.frame(x = 1:10), 1:6)
    Error <dplyr_error>
      `slice_max(order_by=)` must have size 10, not size 6.

# slice_sample() check size of `weight_by=` (#5922)

    Code
      slice_sample(data.frame(x = 1:10), n = 2, weight_by = 1:6)
    Error <dplyr_error>
      incorrect number of probabilities

# rename errors with invalid grouped data frame (#640)

    Code
      slice(df, TRUE)
    Error <dplyr_error>
      `slice()` expressions should return indices (positive or negative integers).

---

    Code
      slice(df, FALSE)
    Error <dplyr_error>
      `slice()` expressions should return indices (positive or negative integers).

---

    Code
      mtcars %>% slice(c(-1, 2))
    Error <dplyr_error>
      `slice()` expressions should return either all positive or all negative.

---

    Code
      mtcars %>% slice(c(2:3, -1))
    Error <dplyr_error>
      `slice()` expressions should return either all positive or all negative.

---

    Code
      check_slice_size(n = 1, prop = 1)
    Error <rlang_error>
      Must supply exactly one of `n` and `prop` arguments.

---

    Code
      check_slice_size(n = "a")
    Error <rlang_error>
      `n` must be a single number.

---

    Code
      check_slice_size(prop = "a")
    Error <rlang_error>
      `prop` must be a single number.

---

    Code
      check_slice_size(n = n())
    Error <rlang_error>
      `n` must be a constant in `check_slice_size()`.
      x `n()` must only be used inside dplyr verbs.

---

    Code
      check_slice_size(prop = n())
    Error <rlang_error>
      `prop` must be a constant in `check_slice_size()`.
      x `n()` must only be used inside dplyr verbs.

---

    Code
      check_slice_size(n = NA)
    Error <rlang_error>
      `n` must be a single number.

---

    Code
      check_slice_size(prop = NA)
    Error <rlang_error>
      `prop` must be a single number.

