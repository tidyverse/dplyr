# order_by() gives useful error messages

    Code
      (expect_error(order_by(mtcars, 10)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not the number 10.
    Code
      (expect_error(order_by(mtcars, cyl)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not a symbol.
      i Did you mean `arrange(mtcars, cyl)`?

# `with_order()` requires `order_by` and `x` to be the same size

    Code
      with_order(1:2, identity, 1:3)
    Condition
      Error in `with_order()`:
      ! `order_by` must have size 3, not size 2.

# order_by() give meaningful errors

    Code
      (expect_error(order_by(NULL, 1L)))
    Output
      <error/rlang_error>
      Error in `order_by()`:
      ! `call` must be a function call, not the number 1.

