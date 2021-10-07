# all_exprs() gives meaningful error messages

    Code
      (expect_error(all_exprs()))
    Output
      <error/rlang_error>
      Error in `quo_reduce()`: At least one expression must be given.
    Code
      (expect_error(any_exprs()))
    Output
      <error/rlang_error>
      Error in `quo_reduce()`: At least one expression must be given.

