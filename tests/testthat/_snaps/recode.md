# recode() gives meaningful error messages

    Code
      (expect_error(recode(factor("a"), a = 5, .missing = 10)))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! `.missing` is not supported for factors.
    Code
      (expect_error(recode("a", b = 5, "c")))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! Argument 3 must be named.
    Code
      (expect_error(recode(factor("a"), b = 5, "c")))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! Argument 3 must be named.
    Code
      (expect_error(recode(1:5)))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! No replacements provided.
    Code
      (expect_error(recode("a")))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! No replacements provided.
    Code
      (expect_error(recode(factor("a"))))
    Output
      <error/rlang_error>
      Error in `recode()`:
      ! No replacements provided.

