# n_distinct() generates useful errors

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
      Error in `vctrs::data_frame()`:
      ! `..1` must be a vector, not a function.

