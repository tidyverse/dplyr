# do() gives meaningful error messages

    Code
      (expect_error(do(df, head, tail)))
    Output
      <error/rlang_error>
      Error in `do()`:
      ! Can only supply one unnamed argument, not 2.
    Code
      (expect_error(do(ungroup(df), 1)))
    Output
      <error/rlang_error>
      Error in `do()`:
      ! Result must be a data frame, not numeric.
    Code
      (expect_error(do(df, 1)))
    Output
      <error/rlang_error>
      Error in `do()`:
      ! Results 1, 2, 3 must be data frames, not numeric.
    Code
      (expect_error(do(df, "a")))
    Output
      <error/rlang_error>
      Error in `do()`:
      ! Results 1, 2, 3 must be data frames, not character.
    Code
      (expect_error(do(df, x = 1, 2)))
    Output
      <error/rlang_error>
      Error in `do()`:
      ! Arguments must either be all named or all unnamed.

