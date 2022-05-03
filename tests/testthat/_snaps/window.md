# order_by() give meaningful errors

    Code
      (expect_error(order_by(NULL, 1L)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not an integer vector.

