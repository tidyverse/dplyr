# give useful error messages when not applicable

    Code
      (expect_error(n()))
    Output
      <error/rlang_error>
      Error in `n()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
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
    Code
      (expect_error(cur_column()))
    Output
      <error/rlang_error>
      Error in `cur_column()`:
      ! Must only be used inside `across()`.
    Code
      (expect_error(cur_group()))
    Output
      <error/rlang_error>
      Error in `cur_group()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_group_id()))
    Output
      <error/rlang_error>
      Error in `cur_group_id()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_group_rows()))
    Output
      <error/rlang_error>
      Error in `cur_group_rows()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

