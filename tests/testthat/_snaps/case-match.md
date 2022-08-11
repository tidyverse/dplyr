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

