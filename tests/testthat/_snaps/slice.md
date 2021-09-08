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

