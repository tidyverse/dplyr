# count() give meaningful errors

    Code
      (expect_error(union(tibble(a = 1), tibble(a = "1"))))
    Output
      <error/rlang_error>
      Error in `union()`: `x` and `y` are not compatible: 
      - Incompatible types for column `a`: double vs character
    Code
      (expect_error(union(tibble(a = 1, b = 2), tibble(a = "1", b = "2"))))
    Output
      <error/rlang_error>
      Error in `union()`: `x` and `y` are not compatible: 
      - Incompatible types for column `a`: double vs character
      - Incompatible types for column `b`: double vs character

