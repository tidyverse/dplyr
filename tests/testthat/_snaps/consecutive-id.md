# generates useful errors

    Code
      consecutive_id(x = 1:4)
    Condition
      Error in `consecutive_id()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * x = 1:4
    Code
      consecutive_id(mean)
    Condition
      Error in `vctrs::data_frame()`:
      ! `..1` must be a vector, not a function.

