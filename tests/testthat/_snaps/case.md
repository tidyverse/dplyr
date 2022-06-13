# `mutate_case()` - is type stable on input

    Code
      mutate_case(df, when(x == 1L, x = "x"))
    Condition
      Error in `mutate_case()`:
      ! Can't convert `x` <character> to <integer>.

# `mutate_case()` - is size stable on input

    Code
      mutate_case(df, when(x >= 2, x = 5:7))
    Condition
      Error in `mutate_case()`:
      ! Problem while computing `x = 5:7`.
      x `x` must be size 2 or 1, not 3.

# conditions must be logical vectors

    Code
      mutate_case(df, when(x + 1, x = x + 2))
    Condition
      Error in `mutate_case()`:
      ! Problem while computing `..1 = x + 1`.
      x Input `..1` must be a logical vector, not a double.

---

    Code
      mutate_case(df, when(x == 1, x = x + 2), when(x + 99, x = x + 2))
    Condition
      Error in `mutate_case()`:
      ! Problem while computing `..1 = x + 99`.
      x Input `..1` must be a logical vector, not a double.

# `mutate_case()` - enforce that columns can't be removed

    Code
      mutate_case(df, when(x > 5, y = NULL))
    Condition
      Error in `mutate_case()`:
      ! Can't delete columns.

---

    Code
      mutate_case(df, when(x > 5, x = 1), when(y < 5, x = 2, y = NULL))
    Condition
      Error in `mutate_case()`:
      ! Can't delete columns.

