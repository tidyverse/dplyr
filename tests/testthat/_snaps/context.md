# give useful error messages when not applicable

    Code
      (expect_error(n()))
    Output
      <error/rlang_error>
      Error in `n()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(cur_data()))
    Output
      <error/rlang_error>
      Error in `cur_data()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(cur_data_all()))
    Output
      <error/rlang_error>
      Error in `cur_data_all()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(cur_column()))
    Output
      <error/rlang_error>
      Error in `cur_column()`:
      ! Must be used inside `across()`.
    Code
      (expect_error(cur_group()))
    Output
      <error/rlang_error>
      Error in `cur_group()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(cur_group_id()))
    Output
      <error/rlang_error>
      Error in `cur_group_id()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(cur_group_rows()))
    Output
      <error/rlang_error>
      Error in `cur_group_rows()`:
      ! Must be used inside dplyr verbs.

