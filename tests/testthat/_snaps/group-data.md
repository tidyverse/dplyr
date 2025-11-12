# group_keys(...) is defunct

    Code
      group_keys(df, x)
    Condition
      Error:
      ! The `...` argument of `group_keys()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please `group_by()` first

# no arg group_indices() is deprecated

    Code
      out <- summarise(df, id = group_indices())
    Condition
      Warning:
      There was 1 warning in `summarise()`.
      i In argument: `id = group_indices()`.
      Caused by warning:
      ! `group_indices()` with no arguments was deprecated in dplyr 1.0.0.
      i Please use `cur_group_id()` instead.

# group_indices(...) is deprecated

    Code
      group_indices(df, x)
    Condition
      Error:
      ! The `...` argument of `group_indices()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please `group_by()` first

