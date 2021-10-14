# sample_*() gives meaningful error messages

    Code
      df2 <- tibble(x = rep(1:2, 100), y = rep(c(0, 1), 100), g = rep(1:2, each = 100))
      grp <- df2 %>% group_by(g)
      (expect_error(sample_n(grp, nrow(df2) / 2, weight = y)))
    Output
      <error/dplyr_error>
      Error in `h()`: too few positive probabilities
      i The error occurred in group 1: g = 1.
    Code
      (expect_error(sample_frac(grp, 1, weight = y)))
    Output
      <error/dplyr_error>
      Error in `h()`: too few positive probabilities
      i The error occurred in group 1: g = 1.
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% sample_n(10)))
    Output
      <error/dplyr_error>
      Error: `size` must be less than or equal to 7 (size of data), set `replace` = TRUE to use sampling with replacement.
      i The error occurred in group 2: cyl = 6.
    Code
      (expect_error(sample_n(list())))
    Output
      <error/rlang_error>
      Error in `sample_n.default()`: `tbl` must be a data frame, not a list.
    Code
      (expect_error(sample_frac(list())))
    Output
      <error/rlang_error>
      Error in `sample_frac.default()`: `tbl` must be a data frame, not a list.
    Code
      (expect_error(check_weight(letters[1:2], 2)))
    Output
      <error/rlang_error>
      Error in `check_weight()`: `weight` must be a numeric, not a character vector.
    Code
      (expect_error(check_weight(-1:-2, 2)))
    Output
      <error/rlang_error>
      Error in `check_weight()`: `weight` must be a vector with all values nonnegative, not -1.
    Code
      (expect_error(check_weight(letters, 2)))
    Output
      <error/rlang_error>
      Error in `check_weight()`: `weight` must be a numeric, not a character vector.
    Code
      # # respects weight
      df <- data.frame(x = 1:2, y = c(0, 1))
      (expect_error(sample_n(df, 2, weight = y)))
    Output
      <error/dplyr_error>
      Error in `h()`: too few positive probabilities
    Code
      (expect_error(sample_frac(df, 2)))
    Output
      <error/dplyr_error>
      Error: `size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement.
    Code
      (expect_error(sample_frac(df %>% group_by(y), 2)))
    Output
      <error/dplyr_error>
      Error: `size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement.
      i The error occurred in group 1: y = 0.
    Code
      (expect_error(sample_frac(df, 1, weight = y)))
    Output
      <error/dplyr_error>
      Error in `h()`: too few positive probabilities

