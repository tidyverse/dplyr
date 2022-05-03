# rowwise has decent print method

    Code
      rf
    Output
      # A tibble: 5 x 1
      # Rowwise:  x
            x
        <int>
      1     1
      2     2
      3     3
      4     4
      5     5

# validate_rowwise_df() gives useful errors

    Code
      (expect_error(validate_rowwise_df(df1)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `.rows` column must be a list of size 1, one-based integer vectors with the right value.
    Code
      (expect_error(validate_rowwise_df(df2)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The last column of the `groups` attribute must be called `.rows`.
    Code
      (expect_error(validate_rowwise_df(df3)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `groups` attribute must be a data frame.
    Code
      (expect_error(validate_rowwise_df(df4)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `groups` attribute must be a data frame.
    Code
      (expect_error(validate_rowwise_df(df7)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `.rows` column must be a list of size 1, one-based integer vectors with the right value.
    Code
      (expect_error(attr(df8, "groups")$.rows <- 1:8))
    Output
      <error/tibble_error_assign_incompatible_size>
      Error:
      ! Assigned data `1:8` must be compatible with existing data.
      x Existing data has 10 rows.
      x Assigned data has 8 rows.
      i Only vectors of size 1 are recycled.
    Code
      (expect_error(validate_rowwise_df(df10)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `groups` attribute must be a data frame.
    Code
      (expect_error(validate_rowwise_df(df11)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`:
      ! The `groups` attribute must be a data frame.
    Code
      (expect_error(new_rowwise_df(tibble(x = 1:10), tibble(".rows" := list(1:5, -1L))))
      )
    Output
      <error/rlang_error>
      Error in `new_rowwise_df()`:
      ! `group_data` must be a tibble without a `.rows` column.
    Code
      (expect_error(new_rowwise_df(tibble(x = 1:10), 1:10)))
    Output
      <error/rlang_error>
      Error in `new_rowwise_df()`:
      ! `group_data` must be a tibble without a `.rows` column.

