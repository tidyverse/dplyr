# n_distinct() generates useful errors

    Code
      n_distinct()
    Condition
      Error in `n_distinct()`:
      ! `...` is absent, but must be supplied.
    Code
      n_distinct(x = 1:4)
    Condition
      Error in `n_distinct()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * x = 1:4
    Code
      n_distinct(mean)
    Condition
      Error in `n_distinct()`:
      ! `..1` must be a vector, not a function.

