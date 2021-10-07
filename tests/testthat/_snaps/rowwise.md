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
      df1 <- rowwise(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
      groups <- attr(df1, "groups")
      groups[[2]] <- 4:1
      attr(df1, "groups") <- groups
      (expect_error(validate_rowwise_df(df1)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: `.rows` column is not a list of size 1, one-based integer vectors with the right value.
    Code
      df2 <- rowwise(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
      groups <- attr(df2, "groups")
      names(groups) <- c("g", "not.rows")
      attr(df2, "groups") <- groups
      (expect_error(validate_rowwise_df(df2)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      df3 <- df2
      attr(df3, "groups") <- tibble()
      (expect_error(validate_rowwise_df(df3)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      df4 <- df3
      attr(df4, "groups") <- NA
      (expect_error(validate_rowwise_df(df4)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      df7 <- rowwise(tibble(x = 1:10))
      attr(df7, "groups")$.rows <- 11:20
      (expect_error(validate_rowwise_df(df7)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: `.rows` column is not a list of size 1, one-based integer vectors with the right value.
    Code
      df8 <- rowwise(tibble(x = 1:10))
      (expect_error(attr(df8, "groups")$.rows <- 1:8))
    Output
      <error/tibble_error_assign_incompatible_size>
      Error: Assigned data `1:8` must be compatible with existing data.
      x Existing data has 10 rows.
      x Assigned data has 8 rows.
      i Only vectors of size 1 are recycled.
    Code
      df10 <- df7
      attr(df10, "groups") <- tibble()
      (expect_error(validate_rowwise_df(df10)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      df11 <- df7
      attr(df11, "groups") <- NULL
      (expect_error(validate_rowwise_df(df11)))
    Output
      <error/rlang_error>
      Error in `validate_rowwise_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(new_rowwise_df(tibble(x = 1:10), tibble(".rows" := list(1:5, -1L))))
      )
    Output
      <error/rlang_error>
      Error in `new_rowwise_df()`: `group_data` must be a tibble without a `.rows` column.
    Code
      (expect_error(new_rowwise_df(tibble(x = 1:10), 1:10)))
    Output
      <error/rlang_error>
      Error in `new_rowwise_df()`: `group_data` must be a tibble without a `.rows` column.

