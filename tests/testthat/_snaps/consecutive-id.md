# follows recycling rules

    Code
      consecutive_id(1:3, 1:4)
    Condition
      Error in `consecutive_id()`:
      ! Can't recycle `..1` (size 3) to match `..2` (size 4).

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
      Error in `consecutive_id()`:
      ! `..1` must be a vector, not a function.

