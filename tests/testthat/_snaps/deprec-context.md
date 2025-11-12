# cur_data() is deprecated

    Code
      mutate(df, y = cur_data())
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `y = cur_data()`.
      Caused by warning:
      ! `cur_data()` was deprecated in dplyr 1.1.0.
      i Please use `pick()` instead.
    Output
      # A tibble: 1 x 2
            x   y$x
        <dbl> <dbl>
      1     1     1

# cur_data_all() is deprecated

    Code
      mutate(df, y = cur_data_all())
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `y = cur_data_all()`.
      Caused by warning:
      ! `cur_data_all()` was deprecated in dplyr 1.1.0.
      i Please use `pick()` instead.
    Output
      # A tibble: 1 x 2
            x   y$x
        <dbl> <dbl>
      1     1     1

# give useful error messages when not applicable

    Code
      (expect_error(cur_data()))
    Output
      <error/rlang_error>
      Error in `cur_data()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_data_all()))
    Output
      <error/rlang_error>
      Error in `cur_data_all()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

