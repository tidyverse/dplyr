# case_when() conditions must be logical (and aren't cast to logical!)

    Code
      case_when(1 ~ 2)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not the number 1.

---

    Code
      case_when(TRUE ~ 2, 3.5 ~ 4)
    Condition
      Error in `case_when()`:
      ! `..2 (left)` must be a logical vector, not the number 3.5.

# case_when() does not accept classed logical conditions

    Code
      case_when(x ~ 1)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not a <my_logical> object.

# case_when() logical conditions can't be arrays (#6862)

    Code
      case_when(x ~ y)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not a logical matrix.

---

    Code
      case_when(x ~ y)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not a logical 1D array.

# `.default` isn't part of recycling

    Code
      case_when(FALSE ~ 1L, .default = 2:5)
    Condition
      Error in `case_when()`:
      ! Can't recycle `.default` (size 4) to size 1.

# `.default` is part of common type computation

    Code
      case_when(TRUE ~ 1L, .default = "x")
    Condition
      Error in `case_when()`:
      ! Can't combine <integer> and `.default` <character>.

# passes through `.ptype` correctly

    Code
      case_when(TRUE ~ 1, FALSE ~ 1.5, .ptype = integer())
    Condition
      Error in `case_when()`:
      ! Can't convert from `..2 (right)` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      case_when(TRUE ~ 1, NULL, FALSE ~ 1.5, .ptype = integer())
    Condition
      Error in `case_when()`:
      ! Can't convert from `..3 (right)` <double> to <integer> due to loss of precision.
      * Locations: 1

# passes through `.size` correctly

    Code
      case_when(TRUE ~ 1:2, .size = 3)
    Condition
      Error in `case_when()`:
      ! Can't recycle `..1 (right)` (size 2) to size 3.

---

    Code
      case_when(TRUE ~ 1:3, NULL, TRUE ~ 1:2, .size = 3)
    Condition
      Error in `case_when()`:
      ! Can't recycle `..3 (right)` (size 2) to size 3.

# can't supply `.default` and `.unmatched`

    Code
      case_when(TRUE ~ 1, .default = 1, .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Can't set `.default` when `unmatched = "error"`.

# `.unmatched` is validated

    Code
      case_when(TRUE ~ 1, .unmatched = "foo")
    Condition
      Error in `case_when()`:
      ! `unmatched` must be either "default" or "error", not "foo".

---

    Code
      case_when(TRUE ~ 1, .unmatched = 1)
    Condition
      Error in `case_when()`:
      ! `unmatched` must be a string, not the number 1.

# `.unmatched` treats `FALSE` like an unmatched location

    Code
      case_when(c(TRUE, FALSE, TRUE) ~ 1, .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

# `.unmatched` treats `NA` like an unmatched location

    Code
      case_when(c(TRUE, NA, TRUE) ~ 1, .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

# `.unmatched` errors pluralize well

    Code
      case_when(x == "a" ~ 1, x == "b" ~ 2, x == "c" ~ 3, x == "e" ~ 4, .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Each location must be matched.
      x Location 4 is unmatched.

---

    Code
      case_when(x == "a" ~ 1, x == "c" ~ 2, x == "e" ~ 3, .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Each location must be matched.
      x Locations 2 and 4 are unmatched.

---

    Code
      case_when(x == 1 ~ "a", .unmatched = "error")
    Condition
      Error in `case_when()`:
      ! Each location must be matched.
      x Locations 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 99, and 100 are unmatched.

# invalid type errors are correct (#6261) (#6206)

    Code
      case_when(TRUE ~ 1, TRUE ~ "x")
    Condition
      Error in `case_when()`:
      ! Can't combine `..1 (right)` <double> and `..2 (right)` <character>.

# `NULL` formula element throws meaningful error (#7794)

    Code
      case_when(NULL ~ NULL)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not `NULL`.
    Code
      case_when(TRUE ~ NULL)
    Condition
      Error in `case_when()`:
      ! `..1 (right)` must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
    Code
      case_when(NULL ~ TRUE)
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not `NULL`.
    Code
      case_when(c(TRUE, TRUE) ~ NULL)
    Condition
      Error in `case_when()`:
      ! `..1 (right)` must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
    Code
      case_when(NULL ~ c(TRUE, TRUE))
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not `NULL`.
    Code
      case_when(TRUE ~ NULL, c(TRUE, TRUE) ~ NULL)
    Condition
      Error in `case_when()`:
      ! `..1 (right)` must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
    Code
      case_when(NULL ~ TRUE, NULL ~ c(TRUE, TRUE))
    Condition
      Error in `case_when()`:
      ! `..1 (left)` must be a logical vector, not `NULL`.

---

    Code
      case_when(c(TRUE, TRUE) ~ NULL, c(TRUE, TRUE, TRUE) ~ NULL)
    Condition
      Error in `case_when()`:
      ! Can't recycle `..1 (left)` (size 2) to match `..2 (left)` (size 3).
    Code
      case_when(NULL ~ c(TRUE, TRUE), NULL ~ c(TRUE, TRUE, TRUE))
    Condition
      Error in `case_when()`:
      ! Can't recycle `..1 (right)` (size 2) to match `..2 (right)` (size 3).

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
      ! `...` can't be empty.
    Code
      (expect_error(case_when(NULL)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! `...` can't be empty.
    Code
      (expect_error(case_when(~ 1:2)))
    Output
      <error/rlang_error>
      Error in `case_when()`:
      ! Case 1 (`~1:2`) must be a two-sided formula, not a one-sided formula.

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

# replace_when() allows vector RHS of the same size as `x`

    Code
      replace_when(x, x == 1 ~ 1:3)
    Condition
      Error in `replace_when()`:
      ! Can't recycle `..1 (right)` (size 3) to size 6.

# replace_when() does not recycle LHS values

    Code
      replace_when(x, TRUE ~ 0)
    Condition
      Error in `replace_when()`:
      ! `..1 (left)` must have size 3, not size 1.

---

    Code
      replace_when(x, c(TRUE, TRUE, TRUE) ~ 0, NULL, TRUE ~ 0)
    Condition
      Error in `replace_when()`:
      ! `..3 (left)` must have size 3, not size 1.

# replace_when() retains the type of `x`

    Code
      replace_when(x, x == "a" ~ "d")
    Condition
      Error in `replace_when()`:
      ! Can't convert from `..1 (right)` <character> to <factor<af15a>> due to loss of generality.
      * Locations: 1

---

    Code
      replace_when(x, x == "a" ~ "b", NULL, x == "b" ~ "d")
    Condition
      Error in `replace_when()`:
      ! Can't convert from `..3 (right)` <character> to <factor<af15a>> due to loss of generality.
      * Locations: 1

# replace_when() does not allow named `...`

    Code
      replace_when(1, foo = TRUE ~ 2)
    Condition
      Error in `replace_when()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * foo = TRUE ~ 2

