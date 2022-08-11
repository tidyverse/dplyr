# requires at least one condition

    Code
      case_switch(1)
    Condition
      Error in `case_switch()`:
      ! At least one condition must be supplied.

---

    Code
      case_switch(1, NULL)
    Condition
      Error in `case_switch()`:
      ! At least one condition must be supplied.

# `.default` is part of common type computation

    Code
      case_switch(1, 1 ~ 1L, .default = "x")
    Condition
      Error in `case_switch()`:
      ! Can't combine `1L` <integer> and `.default` <character>.

