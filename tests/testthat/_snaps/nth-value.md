# nth() gives meaningful error message (#5466)

    Code
      (expect_error(nth(1:10, "x")))
    Output
      <error/rlang_error>
      Error in `nth()`:
      ! `n` must be a single integer.

