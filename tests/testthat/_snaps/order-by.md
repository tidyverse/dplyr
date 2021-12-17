# order_by() gives useful error messages

    Code
      (expect_error(order_by(mtcars, 10)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not a double vector.
    Code
      (expect_error(order_by(mtcars, cyl)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not a symbol.
      i Did you mean `arrange(mtcars, cyl)`?

