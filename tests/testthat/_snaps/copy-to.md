# `auto_copy()` throws an informative error on different sources (#6798)

    Code
      auto_copy(df, NULL)
    Condition
      Error in `auto_copy()`:
      ! `x` and `y` must share the same src.
      i `x` is a <tbl_df/tbl/data.frame> object.
      i `y` is `NULL`.
      i Set `copy = TRUE` if `y` can be copied to the same source as `x` (may be slow).

