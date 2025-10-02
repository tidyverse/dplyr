# `.default` isn't part of recycling

    Code
      case_when(FALSE ~ 1L, .default = 2:5)
    Condition
      Error in `case_when()`:
      ! `.default` must have size 1, not size 4.

# `.default` is part of common type computation

    Code
      case_when(TRUE ~ 1L, .default = "x")
    Condition
      Error in `case_when()`:
      ! Can't combine `..1 (right)` <integer> and `.default` <character>.

# passes through `.size` correctly

    Code
      case_when(TRUE ~ 1:2, .size = 3)
    Condition
      Error in `case_when()`:
      ! `..1 (right)` must have size 3, not size 2.

# invalid type errors are correct (#6261) (#6206)

    Code
      case_when(TRUE ~ 1, TRUE ~ "x")
    Condition
      Error in `case_when()`:
      ! Can't combine `..1 (right)` <double> and `..2 (right)` <character>.

# `NULL` formula element throws meaningful error

    Code
      case_when(1 ~ NULL)
    Condition
      Warning:
      Calling `case_when()` with size 1 LHS inputs and size >1 RHS inputs was deprecated in dplyr 1.2.0.
      i This `case_when()` statement can result in subtle silent bugs and is very inefficient.
      
        Please use a series of if statements instead:
      
        ```
        # Previously
        case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)
      
        # Now
        if (scalar_lhs1) {
          rhs1
        } else if (scalar_lhs2) {
          rhs2
        } else {
          default
        }
        ```
      Error in `case_when()`:
      ! `..1 (right)` must be a vector, not `NULL`.

---

    Code
      case_when(NULL ~ 1)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not `NULL`.

# throws chained errors when formula evaluation fails

    Code
      case_when(1 ~ 2, 3 ~ stop("oh no!"))
    Condition
      Error in `case_when()`:
      ! Failed to evaluate the right-hand side of formula 2.
      Caused by error:
      ! oh no!

---

    Code
      case_when(1 ~ 2, stop("oh no!") ~ 4)
    Condition
      Error in `case_when()`:
      ! Failed to evaluate the left-hand side of formula 2.
      Caused by error:
      ! oh no!

# case_when() give meaningful errors

    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1:3, c(FALSE, TRUE) ~ 1:2)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `case_when()`:
      ! Can't recycle `..1 (right)` (size 3) to match `..2 (right)` (size 2).
    Code
      (expect_error(case_when(c(TRUE, FALSE) ~ 1, c(FALSE, TRUE, FALSE) ~ 2, c(FALSE,
        TRUE, FALSE, NA) ~ 3)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `case_when()`:
      ! Can't recycle `..1 (left)` (size 2) to match `..2 (left)` (size 3).
    Code
      (expect_error(case_when(51:53 ~ 1:3)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not an integer vector.
    Code
      (expect_error(case_when(paste(50))))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Case 1 (`paste(50)`) must be a two-sided formula, not the string "50".
    Code
      (expect_error(case_when(y ~ x, paste(50))))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Case 2 (`paste(50)`) must be a two-sided formula, not the string "50".
    Code
      (expect_error(case_when()))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! At least one condition must be supplied.
    Code
      (expect_error(case_when(NULL)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! At least one condition must be supplied.
    Code
      (expect_error(case_when(~ 1:2)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Case 1 (`~1:2`) must be a two-sided formula.

# Using scalar LHS with vector RHS is deprecated (#7082)

    Code
      x <- 1:5
      y <- 6:10
      code <- 1L
      sex <- "M"
      expect_identical(case_when(code == 1L && sex == "M" ~ x, code == 1L && sex ==
        "F" ~ y, code == 1L && sex == "M" ~ x + 1L, .default = 0L), x)
    Condition
      Warning:
      Calling `case_when()` with size 1 LHS inputs and size >1 RHS inputs was deprecated in dplyr 1.2.0.
      i This `case_when()` statement can result in subtle silent bugs and is very inefficient.
      
        Please use a series of if statements instead:
      
        ```
        # Previously
        case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)
      
        # Now
        if (scalar_lhs1) {
          rhs1
        } else if (scalar_lhs2) {
          rhs2
        } else {
          default
        }
        ```

---

    Code
      x <- 1
      case_when(x == 1 ~ "a", x == 2 ~ character(), .default = "other")
    Condition
      Warning:
      Calling `case_when()` with size 1 LHS inputs and size >1 RHS inputs was deprecated in dplyr 1.2.0.
      i This `case_when()` statement can result in subtle silent bugs and is very inefficient.
      
        Please use a series of if statements instead:
      
        ```
        # Previously
        case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)
      
        # Now
        if (scalar_lhs1) {
          rhs1
        } else if (scalar_lhs2) {
          rhs2
        } else {
          default
        }
        ```
    Output
      character(0)

