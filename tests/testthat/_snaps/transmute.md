# transmute() error messages

    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .keep = "all")))
    Output
      <error/rlang_error>
      Error in `check_transmute_args()`: `transmute()` does not support the `.keep` argument
    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .before = disp)))
    Output
      <error/rlang_error>
      Error in `check_transmute_args()`: `transmute()` does not support the `.before` argument
    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .after = disp)))
    Output
      <error/rlang_error>
      Error in `check_transmute_args()`: `transmute()` does not support the `.after` argument

