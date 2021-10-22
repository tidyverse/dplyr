# slice_*() checks for empty ...

    Code
      (expect_error(slice_head(df, 5)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `slice_head()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?
    Code
      (expect_error(slice_tail(df, 5)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `slice_tail()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?
    Code
      (expect_error(slice_min(df, x, 5)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `slice_min()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?
    Code
      (expect_error(slice_max(df, x, 5)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `slice_max()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?
    Code
      (expect_error(slice_sample(df, 5)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `slice_sample()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?

# slice_*() checks for constant n= and prop=

    Code
      (expect_error(slice_head(df, n = n())))
    Output
      <error/rlang_error>
      Error in `slice_head()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_head(df, prop = n())))
    Output
      <error/rlang_error>
      Error in `slice_head()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_tail(df, n = n())))
    Output
      <error/rlang_error>
      Error in `slice_tail()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_tail(df, prop = n())))
    Output
      <error/rlang_error>
      Error in `slice_tail()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_min(df, x, n = n())))
    Output
      <error/rlang_error>
      Error in `slice_min()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_min(df, x, prop = n())))
    Output
      <error/rlang_error>
      Error in `slice_min()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_max(df, x, n = n())))
    Output
      <error/rlang_error>
      Error in `slice_max()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_max(df, x, prop = n())))
    Output
      <error/rlang_error>
      Error in `slice_max()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_sample(df, n = n())))
    Output
      <error/rlang_error>
      Error in `slice_sample()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(slice_sample(df, prop = n())))
    Output
      <error/rlang_error>
      Error in `slice_sample()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.

# slice_min/max() check size of `order_by=` (#5922)

    Code
      (expect_error(slice_min(data.frame(x = 1:10), 1:6)))
    Output
      <error/rlang_error>
      Error in `slice_min()`: `order_by` must have size 10, not size 6.
    Code
      (expect_error(slice_max(data.frame(x = 1:10), 1:6)))
    Output
      <error/rlang_error>
      Error in `slice_max()`: `order_by` must have size 10, not size 6.

# slice_sample() check size of `weight_by=` (#5922)

    Code
      (expect_error(slice_sample(data.frame(x = 1:10), n = 2, weight_by = 1:6)))
    Output
      <error/rlang_error>
      Error in `slice_sample()`: `weight_by` must have size 10, not size 6.

# rename errors with invalid grouped data frame (#640)

    Code
      df <- tibble(x = 1:3)
      (expect_error(slice(df, TRUE)))
    Output
      <error/rlang_error>
      Error in `slice()`: Invalid result of type <logical>.
      i Indices must be positive or negative integers.
    Code
      (expect_error(slice(df, FALSE)))
    Output
      <error/rlang_error>
      Error in `slice()`: Invalid result of type <logical>.
      i Indices must be positive or negative integers.
    Code
      (expect_error(mtcars %>% slice(c(-1, 2))))
    Output
      <error/dplyr_error>
      Error in `slice()`: Indices must be all positive or all negative.
      i Got 1 positives, 1 negatives.
    Code
      (expect_error(mtcars %>% slice(c(2:3, -1))))
    Output
      <error/dplyr_error>
      Error in `slice()`: Indices must be all positive or all negative.
      i Got 2 positives, 1 negatives.
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
      Error in `check_slice_size()`: 
        `n` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
    Code
      (expect_error(check_slice_size(prop = n())))
    Output
      <error/rlang_error>
      Error in `check_slice_size()`: 
        `prop` must be a constant.
      Caused by error in `n()`: 
        Must be used inside dplyr verbs.
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

