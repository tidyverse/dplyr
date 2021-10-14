# emits useful messages

    Code
      (expect_error(join_cols(c("x", "y"), c("y", "y"))))
    Output
      <error/rlang_error>
      Error in `check_duplicate_vars()`: Input columns in `y` must be unique.
      x Problem with `y`.
    Code
      (expect_error(join_cols(c("y", "y"), c("x", "y"))))
    Output
      <error/rlang_error>
      Error in `check_duplicate_vars()`: Input columns in `x` must be unique.
      x Problem with `y`.
    Code
      xy <- c("x", "y")
      vars <- join_cols(xy, xy)
    Message <rlang_message>
      Joining, by = c("x", "y")
    Code
      (expect_error(join_cols(xy, c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `standardise_join_by()`: `by` must be supplied when `x` and `y` have no common variables.
      i use by = character()` to perform a cross-join.
    Code
      (expect_error(join_cols(xy, xy, by = FALSE)))
    Output
      <error/rlang_error>
      Error in `glubort()`: `by` must be a (named) character vector, list, or NULL, not a logical vector.
    Code
      (expect_error(join_cols(xy, xy, by = list(1, 2))))
    Output
      <error/rlang_error>
      Error in `check_join_vars()`: join columns must be character vectors.
    Code
      (expect_error(join_cols(xy, xy, by = c("x", "x"))))
    Output
      <error/rlang_error>
      Error in `check_join_vars()`: Join columns must be unique.
      x Problem at position 2.
    Code
      (expect_error(join_cols(xy, xy, by = c("x", NA))))
    Output
      <error/rlang_error>
      Error in `check_join_vars()`: Join columns must be not NA.
      x Problem at position 2.
    Code
      (expect_error(join_cols(xy, xy, by = c("aaa", "bbb"))))
    Output
      <error/rlang_error>
      Error in `check_join_vars()`: Join columns must be present in data.
      x Problem with `aaa` and `bbb`.
    Code
      (expect_error(join_cols(xy, xy, by = "x", suffix = "x")))
    Output
      <error/rlang_error>
      Error in `standardise_join_suffix()`: `suffix` must be a character vector of length 2.
      i suffix is a character vector of length 1.
    Code
      (expect_error(join_cols(xy, xy, by = "x", suffix = c("", NA))))
    Output
      <error/rlang_error>
      Error in `glubort()`: `suffix` can't be NA.

