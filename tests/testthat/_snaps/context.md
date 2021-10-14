# give useful error messages when not applicable

    Code
      (expect_error(n()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `n()` must only be used inside dplyr verbs.
    Code
      (expect_error(cur_data()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_data()` must only be used inside dplyr verbs.
    Code
      (expect_error(cur_data_all()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_data_all()` must only be used inside dplyr verbs.
    Code
      (expect_error(cur_column()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_column()` must only be used inside `across()`.
    Code
      (expect_error(cur_group()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_group()` must only be used inside dplyr verbs.
    Code
      (expect_error(cur_group_id()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_group_id()` must only be used inside dplyr verbs.
    Code
      (expect_error(cur_group_rows()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `cur_group_rows()` must only be used inside dplyr verbs.

