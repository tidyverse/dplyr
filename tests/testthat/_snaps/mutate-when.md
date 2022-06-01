# `mutate_when()` - is type stable on input

    Code
      mutate_when(df, x == 1L, x = "x")
    Condition
      Error in `mutate_when()`:
      ! Can't convert `x` <character> to <integer>.

# `mutate_when()` - is size stable on input

    Code
      mutate_when(df, x >= 2, x = 5:7)
    Condition
      Error in `mutate_when()`:
      ! Problem while computing `x = 5:7`.
      x `x` must be size 2 or 1, not 3.

# conditions must be logical vectors

    Code
      mutate_when(df, x + 1, x = x + 2)
    Condition
      Error in `mutate_when()`:
      ! Problem while computing `..1 = x + 1`.
      x Input `..1` must be a logical vector, not a double.

---

    Code
      mutate_when(df, x == 1, x = x + 2, x + 99, x = x + 2)
    Condition
      Error in `mutate_when()`:
      ! Problem while computing `..1 = x + 99`.
      x Input `..1` must be a logical vector, not a double.

# `mutate_when()` - enforce that columns can't be removed

    Code
      mutate_when(df, x > 5, y = NULL)
    Condition
      Error in `mutate_when()`:
      ! Can't delete columns.

---

    Code
      mutate_when(df, x > 5, x = 1, y < 5, x = 2, y = NULL)
    Condition
      Error in `mutate_when()`:
      ! Can't delete columns.

# `...` must start with an unnamed input

    Code
      mutate_when(df, x = x)
    Condition
      Error in `mutate_when()`:
      ! The first `...` value must be an unnamed logical condition.

# `revise()` - is type stable on input

    Code
      revise(df, x == 1L, x = "x")
    Condition
      Error:
      ! Can't convert `x` <character> to <integer>.

# `revise()` - is size stable on input

    Code
      revise(df, x >= 2, x = 5:7)
    Condition
      Error:
      ! Problem while computing `x = 5:7`.
      x `x` must be size 2 or 1, not 3.

# `.when` must be a logical vector

    Code
      revise(df, x + 1, x = x + 2)
    Condition
      Error in `revise()`:
      ! Problem while computing `..1 = x + 1`.
      x Input `..1` must be a logical vector, not a double.

# `revise()` - enforce that columns can't be removed

    Code
      revise(df, x > 5, y = NULL)
    Condition
      Error:
      ! Can't delete columns.

---

    Code
      revise(df, x > 5, x = 1, y = NULL)
    Condition
      Error:
      ! Can't delete columns.

