# transmute() error messages

    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .keep = "all")))
    Output
      <error/rlang_error>
      Error in `transmute()`:
      ! The `.keep` argument is not supported.
    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .before = disp)))
    Output
      <error/rlang_error>
      Error in `transmute()`:
      ! The `.before` argument is not supported.
    Code
      (expect_error(transmute(mtcars, cyl2 = cyl, .after = disp)))
    Output
      <error/rlang_error>
      Error in `transmute()`:
      ! The `.after` argument is not supported.

