# `.fn` result type is checked (#6561)

    Code
      rename_with(df, fn)
    Condition
      Error in `rename_with()`:
      ! `.fn` must return a character vector, not an integer.

# `.fn` result size is checked (#6561)

    Code
      rename_with(df, fn)
    Condition
      Error in `rename_with()`:
      ! `.fn` must return a vector of length 2, not 3.

# can't rename in `.cols`

    Code
      rename_with(df, toupper, .cols = c(y = x))
    Condition
      Error in `rename_with()`:
      ! Can't rename variables in this context.

