# validate_grouped_df() gives useful errors

    Code
      df1 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
      groups <- attr(df1, "groups")
      groups[[2]] <- 1:2
      attr(df1, "groups") <- groups
      df2 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
      groups <- attr(df2, "groups")
      names(groups) <- c("g", "not.rows")
      attr(df2, "groups") <- groups
      df3 <- df2
      attr(df3, "groups") <- tibble()
      df4 <- df3
      attr(df4, "groups") <- NA
      df5 <- tibble(x = 1:4, g = rep(1:2, each = 2))
      attr(df5, "vars") <- "g"
      attr(df5, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
      df6 <- new_grouped_df(tibble(x = 1:10), groups = tibble(".rows" := list(1:5,
      -1L)))
      df7 <- df6
      attr(df7, "groups")$.rows <- list(11L)
      df8 <- df6
      attr(df8, "groups")$.rows <- list(0L)
      df10 <- df6
      attr(df10, "groups") <- tibble()
      df11 <- df6
      attr(df11, "groups") <- NULL
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

