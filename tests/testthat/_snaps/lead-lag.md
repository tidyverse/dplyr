# lead() / lag() give meaningful errors

    Code
      lead(letters, -1)
    Error <rlang_error>
      `n` must be a nonnegative integer scalar, not a double vector of length 1.

---

    Code
      lead(letters, "1")
    Error <rlang_error>
      `n` must be a nonnegative integer scalar, not a character vector of length 1.

---

    Code
      lag(letters, -1)
    Error <rlang_error>
      `n` must be a nonnegative integer scalar, not a double vector of length 1.

---

    Code
      lag(letters, "1")
    Error <rlang_error>
      `n` must be a nonnegative integer scalar, not a character vector of length 1.

---

    Code
      lag(ts(1:10))
    Error <rlang_error>
      `x` must be a vector, not a ts object, do you want `stats::lag()`?

---

    Code
      lag(c("1", "2", "3"), default = FALSE)
    Error <vctrs_error_incompatible_type>
      Can't combine `default` <logical> and `x` <character>.

---

    Code
      lag(c("1", "2", "3"), default = character())
    Error <rlang_error>
      `default` must be size 1, not size 0

