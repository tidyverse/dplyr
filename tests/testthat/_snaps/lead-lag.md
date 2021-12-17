# lead() / lag() give meaningful errors

    Code
      # # complicance of n argument
      (expect_error(lead(letters, -1)))
    Output
      <error/rlang_error>
      Error in `lead()`:
      ! `n` must be a positive integer, not a double vector of length 1.
    Code
      (expect_error(lead(letters, "1")))
    Output
      <error/rlang_error>
      Error in `lead()`:
      ! `n` must be a positive integer, not a character vector of length 1.
    Code
      (expect_error(lag(letters, -1)))
    Output
      <error/rlang_error>
      Error in `lag()`:
      ! `n` must be a positive integer, not a double vector of length 1.
    Code
      (expect_error(lag(letters, "1")))
    Output
      <error/rlang_error>
      Error in `lag()`:
      ! `n` must be a positive integer, not a character vector of length 1.
    Code
      # # ts
      (expect_error(lag(ts(1:10))))
    Output
      <error/rlang_error>
      Error in `lag()`:
      ! `x` must be a vector, not a ts object, do you want `stats::lag()`?
    Code
      # # incompatible default
      (expect_error(lag(c("1", "2", "3"), default = FALSE)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `lag()`:
      ! Can't combine `default` <logical> and `x` <character>.
    Code
      (expect_error(lead(c("1", "2", "3"), default = FALSE)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `lead()`:
      ! Can't combine `default` <logical> and `x` <character>.
    Code
      (expect_error(lag(c("1", "2", "3"), default = character())))
    Output
      <error/rlang_error>
      Error in `lag()`:
      ! `default` must be size 1, not size 0
    Code
      (expect_error(lead(c("1", "2", "3"), default = character())))
    Output
      <error/rlang_error>
      Error in `lead()`:
      ! `default` must be size 1, not size 0

