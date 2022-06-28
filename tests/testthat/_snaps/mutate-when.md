# can't remove columns when using `.when`

    Code
      mutate(df, .when = x > 1, x = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = NULL`.
      x Can't remove columns when using `.when`.
      i `x` would be removed.

---

    Code
      mutate(df, .when = x > 1, y = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `y = NULL`.
      x Can't remove columns when using `.when`.
      i `y` would be removed.

# grouped - can't remove columns when using `.when`

    Code
      mutate(df, .when = x > 1, x = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = NULL`.
      x Can't remove columns when using `.when`.
      i `x` would be removed.

---

    Code
      mutate(df, .when = x > 1, y = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `y = NULL`.
      x Can't remove columns when using `.when`.
      i `y` would be removed.

# rowwise - can't remove columns when using `.when`

    Code
      mutate(df, .when = x > 1, x = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = NULL`.
      x Can't remove columns when using `.when`.
      i `x` would be removed.

---

    Code
      mutate(df, .when = x > 1, y = NULL)
    Condition
      Error in `mutate()`:
      ! Problem while computing `y = NULL`.
      x Can't remove columns when using `.when`.
      i `y` would be removed.

# original columns in `.data` are type stable when using `.when`

    Code
      mutate(df, .when = x > 1L, x = 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

---

    Code
      mutate(df, .when = x > 1L, x = x + 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = x + 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

# grouped - original columns in `.data` are type stable when using `.when`

    Code
      mutate(df, .when = x > 1L, x = 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

---

    Code
      mutate(df, .when = x > 1L, x = x + 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = x + 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

# rowwise - original columns in `.data` are type stable when using `.when`

    Code
      mutate(df, .when = x > 1L, x = 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

---

    Code
      mutate(df, .when = x > 1L, x = x + 1.5)
    Condition
      Error in `mutate()`:
      ! Problem while computing `x = x + 1.5`.
      x Can't alter the type of `x`.
      i `x` has type <integer>.
      i Attempting to update with incompatible vector of type <double>.

# `.when` errors give context even if they aren't dplyr errors

    Code
      mutate(df, .when = a)
    Condition
      Error in `mutate()`:
      ! Problem while computing `.when = a`.
      Caused by error:
      ! object 'a' not found

# `.when` must evaluate to a logical vector

    Code
      mutate(df, .when = x)
    Condition
      Error in `mutate()`:
      ! Problem while computing `.when = x`.
      x `.when` must evaluate to a logical vector, not a double vector.

# grouped - `.when` must evaluate to a logical vector

    Code
      mutate(df, .when = x)
    Condition
      Error in `mutate()`:
      ! Problem while computing `.when = x`.
      x `.when` must evaluate to a logical vector, not a double vector.

# `.when` must evaluate to a vector of the same size as `.data`

    Code
      mutate(df, .when = TRUE)
    Condition
      Error in `mutate()`:
      ! Problem while computing `.when = TRUE`.
      x `.when` must have the same size as `.data`.
      i `.data` has size 2.
      i `.when` has size 1.

# grouped - `.when` must evaluate to a vector of the same size as `.data`

    Code
      mutate(df, .when = TRUE)
    Condition
      Error in `mutate()`:
      ! Problem while computing `.when = TRUE`.
      x `.when` must have the same size as `.data`.
      i `.data` has size 2.
      i `.when` has size 1.

