# is type stable on input

    Code
      mutate_when(df, x == 1L, x = "x")
    Condition
      Error in `vec_assign()`:
      ! Can't convert <character> to match type of `x` <integer>.

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

# enforce that columns can't be removed

    Code
      mutate_when(df, x > 5, y = NULL)
    Condition
      Error in `mutate_when()`:
      ! Can't delete columns when using `mutate_when()`.

---

    Code
      mutate_when(df, x > 5, x = 1, y < 5, x = 2, y = NULL)
    Condition
      Error in `mutate_when()`:
      ! Can't delete columns when using `mutate_when()`.

# `...` must start with an unnamed input

    Code
      mutate_when(df, x = x)
    Condition
      Error in `mutate_when()`:
      ! The first `...` value must be an unnamed logical condition.

