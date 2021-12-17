# if_else() give meaningful errors

    Code
      (expect_error(if_else(1:10, 1, 2)))
    Output
      <error/rlang_error>
      Error in `if_else()`:
      ! `condition` must be a logical vector, not an integer vector.
    Code
      (expect_error(if_else(1:3 < 2, 1:2, 1:3)))
    Output
      <error/rlang_error>
      Error in `if_else()`:
      ! `true` must be length 3 (length of `condition`) or one, not 2.
    Code
      (expect_error(if_else(1:3 < 2, 1:3, 1:2)))
    Output
      <error/rlang_error>
      Error in `if_else()`:
      ! `false` must be length 3 (length of `condition`) or one, not 2.
    Code
      (expect_error(if_else(1:3 < 2, 1, 1L)))
    Output
      <error/rlang_error>
      Error in `if_else()`:
      ! `false` must be a double vector, not an integer vector.
    Code
      x <- factor("x")
      y <- ordered("x")
      (expect_error(if_else(1:3 < 2, x, y)))
    Output
      <error/rlang_error>
      Error in `if_else()`:
      ! `false` must have class `factor`, not class `ordered/factor`.

