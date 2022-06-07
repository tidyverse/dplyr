# passes through `.size` correctly

    Code
      case_when(TRUE, 1, .size = 2)
    Condition
      Error in `case_when()`:
      ! `..1` must have size 2, not size 1.

# doesn't support quosures in the new interface

    Code
      case_when(!!!fs)
    Condition
      Error:
      ! Case 1 (`!!!fs`) must be a two-sided formula, not a call.

# invalid type errors are correct (#6261) (#6206)

    Code
      case_when(TRUE ~ 1, TRUE ~ "x")
    Condition
      Error in `case_when()`:
      ! Can't combine `TRUE ~ 1` <double> and `TRUE ~ "x"` <character>.

# trying to mix the old interface with new arguments isn't allowed

    Code
      case_when(1 ~ 2, .default = 3)
    Condition
      Error in `case_when()`:
      ! `.default` can only be used with the new interface.

---

    Code
      case_when(1 ~ 2, .missing = 3)
    Condition
      Error in `case_when()`:
      ! `.missing` can only be used with the new interface.

---

    Code
      case_when(1 ~ 2, .ptype = integer())
    Condition
      Error in `case_when()`:
      ! `.ptype` can only be used with the new interface.

---

    Code
      case_when(1 ~ 2, .size = 1)
    Condition
      Error in `case_when()`:
      ! `.size` can only be used with the new interface.

# case_when() give meaningful errors

    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1:3, c(FALSE, TRUE) ~ 1:2)))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `c(TRUE, FALSE) ~ 1:3` (size 2) to match `c(TRUE, FALSE) ~ 1:3` (size 3).
    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1, c(FALSE, TRUE, FALSE) ~ 2, c(FALSE,
        TRUE, FALSE, NA) ~ 3)))
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `c(TRUE, FALSE) ~ 1` (size 2) to match `c(FALSE, TRUE, FALSE) ~ 2` (size 3).
    Code
      (expect_error(case_when(50 ~ 1:3)))
    Output
      <error/vctrs_error_assert_ptype>
      Error in `case_when()`:
      ! `50 ~ 1:3` must be a vector with type <logical>.
      Instead, it has type <double>.
    Code
      (expect_error(case_when(paste(50))))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `...` must contain an even number of inputs.
      i 1 inputs were provided.
    Code
      (expect_error(case_when()))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `...` can't be empty.
    Code
      (expect_error(case_when(~ 1:2)))
    Output
      <error/rlang_error>
      Error:
      ! Formulas must be two-sided.

