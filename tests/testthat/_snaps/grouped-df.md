# validate_grouped_df() gives useful errors

    Code
      (expect_error(validate_grouped_df(df1)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(validate_grouped_df(df2)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(validate_grouped_df(df3)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(validate_grouped_df(df4)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(validate_grouped_df(df5)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: Corrupt `grouped_df` using old (< 0.8.0) format.
      i Strip off old grouping with `ungroup()`.
    Code
      (expect_error(validate_grouped_df(df6, check_bounds = TRUE)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: out of bounds indices.
    Code
      (expect_error(validate_grouped_df(df7, check_bounds = TRUE)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: out of bounds indices.
    Code
      (expect_error(validate_grouped_df(df8, check_bounds = TRUE)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: out of bounds indices.
    Code
      (expect_error(validate_grouped_df(df10)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(validate_grouped_df(df11)))
    Output
      <error/rlang_error>
      Error in `validate_grouped_df()`: The `groups` attribute is not a data frame with its last column called `.rows`.
    Code
      (expect_error(new_grouped_df(tibble(x = 1:10), tibble(other = list(1:2)))))
    Output
      <error/rlang_error>
      Error in `new_grouped_df()`: `new_grouped_df()` incompatible argument.
      i `groups` should be a data frame, and its last column be called `.rows`.
    Code
      (expect_error(new_grouped_df(10)))
    Output
      <error/rlang_error>
      Error in `new_grouped_df()`: `new_grouped_df()` incompatible argument.
      x `x` is not a data frame.

# helper gives meaningful error messages

    Code
      (expect_error(grouped_df(data.frame(x = 1), "y", FALSE)))
    Output
      <error/rlang_error>
      Error in `compute_groups()`: `vars` missing from `data`: `y`.
    Code
      (expect_error(grouped_df(data.frame(x = 1), 1)))
    Output
      <error/rlang_error>
      Error in `grouped_df()`: `vars` must be a character vector.

