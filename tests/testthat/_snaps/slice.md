# slice_min/max() check size of `order_by=` (#5922)

    Code
      (expect_error(slice_min(data.frame(x = 1:10), 1:6)))
    Output
      <error/dplyr_error>
      Error in `slice_min()`: `order_by` must have size 10, not size 6.
    Code
      (expect_error(slice_max(data.frame(x = 1:10), 1:6)))
    Output
      <error/dplyr_error>
      Error in `slice_max()`: `order_by` must have size 10, not size 6.

# slice_sample() check size of `weight_by=` (#5922)

    Code
      (expect_error(slice_sample(data.frame(x = 1:10), n = 2, weight_by = 1:6)))
    Output
      <error/dplyr_error>
      Error in `slice_sample()`: `weight_by` must have size 10, not size 6.

# rename errors with invalid grouped data frame (#640)

    Code
      df <- tibble(x = 1:3)
      (expect_error(slice(df, TRUE)))
    Output
      <error/dplyr_error>
      Error in `slice()`: `slice()` expressions should return indices (positive or negative integers).
    Code
      (expect_error(slice(df, FALSE)))
    Output
      <error/dplyr_error>
      Error in `slice()`: `slice()` expressions should return indices (positive or negative integers).
    Code
      (expect_error(mtcars %>% slice(c(-1, 2))))
    Output
      <error/dplyr_error>
      Error in `slice()`: `slice()` expressions should return either all positive or all negative.
    Code
      (expect_error(mtcars %>% slice(c(2:3, -1))))
    Output
      <error/dplyr_error>
      Error in `slice()`: `slice()` expressions should return either all positive or all negative.
    Code
      (expect_error(check_slice_size(n = 1, prop = 1)))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: Must supply exactly one of `n` and `prop` arguments.
    Code
      (expect_error(check_slice_size(n = "a")))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: `n` must be a single number.
    Code
      (expect_error(check_slice_size(prop = "a")))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: `prop` must be a single number.
    Code
      (expect_error(check_slice_size(n = n())))
    Output
      <error/rlang_error>
      Error: 
        `n` must be a constant in `check_slice_size()`.
        x `n()` must only be used inside dplyr verbs.
      Caused by error in `context_peek()`: 
        `n()` must only be used inside dplyr verbs.
    Code
      (expect_error(check_slice_size(prop = n())))
    Output
      <error/rlang_error>
      Error: 
        `prop` must be a constant in `check_slice_size()`.
        x `n()` must only be used inside dplyr verbs.
      Caused by error in `context_peek()`: 
        `n()` must only be used inside dplyr verbs.
    Code
      (expect_error(check_slice_size(n = NA)))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: `n` must be a single number.
    Code
      (expect_error(check_slice_size(prop = NA)))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: `prop` must be a single number.

