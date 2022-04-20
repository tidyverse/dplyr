# grouping variables preserved with a message, unless already selected (#1511, #5841)

    Code
      res <- select(df, x)
    Message
      Adding missing grouping variables: `g`

---

    Code
      expect_equal(df %>% select(a = c), tibble(b = 2, a = 3) %>% group_by(b))
    Message
      Adding missing grouping variables: `b`
    Code
      expect_equal(df %>% select(b = c), tibble(a = 1, b = 3) %>% group_by(a))
    Message
      Adding missing grouping variables: `a`

# non-syntactic grouping variable is preserved (#1138)

    Code
      df <- tibble(`a b` = 1L) %>% group_by(`a b`) %>% select()
    Message
      Adding missing grouping variables: `a b`

# select() provides informative errors

    Code
      (expect_error(select(mtcars, 1 + "")))
    Output
      <error/rlang_error>
      Error in `select()`:
      ! non-numeric argument to binary operator

# dplyr_col_select() aborts when `[` implementation is broken

    Code
      (expect_error(select(df1, 1:2)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `select()`:
      ! Can't subset columns past the end.
      i Location 2 doesn't exist.
      i There is only 1 column.
    Code
      (expect_error(select(df1, 0)))
    Output
      <error/rlang_error>
      Error in `select()`:
      ! Can't reconstruct data frame.
      x The `[` method for class <dplyr_test_broken_operator/tbl_df/tbl/data.frame> must return a data frame.
      i It returned a <list>.

---

    Code
      (expect_error(select(df1, 2)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `select()`:
      ! Can't subset columns past the end.
      i Location 2 doesn't exist.
      i There is only 1 column.
    Code
      (expect_error(select(df1, 1)))
    Output
      <error/rlang_error>
      Error in `select()`:
      ! Can't reconstruct data frame.
      x The `[` method for class <dplyr_test_broken_operator/tbl_df/tbl/data.frame> must return a data frame.
      i It returned a <list>.
    Code
      (expect_error(select(df2, 1)))
    Output
      <error/rlang_error>
      Error in `select()`:
      ! Can't reconstruct data frame.
      x The `[` method for class <dplyr_test_operator_wrong_size/tbl_df/tbl/data.frame> must return a data frame with 1 column.
      i It returned a <data.frame> of 0 columns.

