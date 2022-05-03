# case_when() give meaningful errors

    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1:3, c(FALSE, TRUE) ~ 1:2)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `c(TRUE, FALSE) ~ 1:3` must be length 2 or one, not 3.
    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1, c(FALSE, TRUE, FALSE) ~ 2, c(FALSE,
        TRUE, FALSE, NA) ~ 3)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `c(FALSE, TRUE, FALSE) ~ 2`, `c(FALSE, TRUE, FALSE, NA) ~ 3` must be length 2 or one, not 3, 4.
    Code
      (expect_error(case_when(50 ~ 1:3)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! LHS of case 1 (`50`) must be a logical vector, not a double vector.
    Code
      (expect_error(case_when(paste(50))))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Case 1 (`paste(50)`) must be a two-sided formula, not a character vector.
    Code
      (expect_error(case_when()))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! No cases provided.
    Code
      (expect_error(case_when(~ 1:2)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Formulas must be two-sided.

