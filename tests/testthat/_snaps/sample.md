# sample_*() gives meaningful error messages

    Code
      df2 <- tibble(x = rep(1:2, 100), y = rep(c(0, 1), 100), g = rep(1:2, each = 100))
      grp <- df2 %>% group_by(g)
      (expect_error(sample_n(grp, nrow(df2) / 2, weight = y)))
    Output
      <error/rlang_error>
      Error in `sample_n()`:
      ! Problem while computing indices.
      i The error occurred in group 1: g = 1.
      Caused by error in `sample.int()`:
      ! too few positive probabilities
    Code
      (expect_error(sample_frac(grp, 1, weight = y)))
    Output
      <error/rlang_error>
      Error in `sample_frac()`:
      ! Problem while computing indices.
      i The error occurred in group 1: g = 1.
      Caused by error in `sample.int()`:
      ! too few positive probabilities
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% sample_n(10)))
    Output
      <error/rlang_error>
      Error in `sample_n()`:
      ! Problem while computing indices.
      i The error occurred in group 2: cyl = 6.
      Caused by error:
      ! `size` must be less than or equal to 7 (size of data).
      i set `replace = TRUE` to use sampling with replacement.
    Code
      (expect_error(sample_n(list())))
    Output
      <error/rlang_error>
      Error in `sample_n()`:
      ! `tbl` must be a data frame, not a list.
    Code
      (expect_error(sample_frac(list())))
    Output
      <error/rlang_error>
      Error in `sample_frac()`:
      ! `tbl` must be a data frame, not a list.
    Code
      # # respects weight
      df <- data.frame(x = 1:2, y = c(0, 1))
      (expect_error(sample_n(df, 2, weight = y)))
    Output
      <error/rlang_error>
      Error in `sample_n()`:
      ! Problem while computing indices.
      Caused by error in `sample.int()`:
      ! too few positive probabilities
    Code
      (expect_error(sample_frac(df, 2)))
    Output
      <error/rlang_error>
      Error in `sample_frac()`:
      ! Problem while computing indices.
      Caused by error:
      ! `size` of sampled fraction must be less or equal to one.
      i set `replace = TRUE` to use sampling with replacement.
    Code
      (expect_error(sample_frac(df %>% group_by(y), 2)))
    Output
      <error/rlang_error>
      Error in `sample_frac()`:
      ! Problem while computing indices.
      i The error occurred in group 1: y = 0.
      Caused by error:
      ! `size` of sampled fraction must be less or equal to one.
      i set `replace = TRUE` to use sampling with replacement.
    Code
      (expect_error(sample_frac(df, 1, weight = y)))
    Output
      <error/rlang_error>
      Error in `sample_frac()`:
      ! Problem while computing indices.
      Caused by error in `sample.int()`:
      ! too few positive probabilities

