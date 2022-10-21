# requires at least one condition

    Code
      case_match(1)
    Condition
      Error in `case_match()`:
      ! At least one condition must be supplied.

---

    Code
      case_match(1, NULL)
    Condition
      Error in `case_match()`:
      ! At least one condition must be supplied.

# `.default` is part of common type computation

    Code
      case_match(1, 1 ~ 1L, .default = "x")
    Condition
      Error in `case_match()`:
      ! Can't combine `1L` <integer> and `.default` <character>.

# `NULL` formula element throws meaningful error

    Code
      case_match(1, 1 ~ NULL)
    Condition
      Error in `case_match()`:
      ! `NULL` must be a vector, not `NULL`.

---

    Code
      case_match(1, NULL ~ 1)
    Condition
      Error in `case_match()`:
      ! `NULL` must be a vector, not `NULL`.

# throws chained errors when formula evaluation fails

    Code
      case_match(1, 1 ~ 2, 3 ~ stop("oh no!"))
    Condition
      Error in `case_match()`:
      ! Failed to evaluate the right-hand side of formula 2.
      Caused by error:
      ! oh no!

---

    Code
      case_match(1, 1 ~ 2, stop("oh no!") ~ 4)
    Condition
      Error in `case_match()`:
      ! Failed to evaluate the left-hand side of formula 2.
      Caused by error:
      ! oh no!

