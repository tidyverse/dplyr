# non-syntactic grouping variable is preserved (#1138)

    Code
      df <- tibble(`a b` = 1L) %>% group_by(`a b`) %>% select()
    Message <message>
      Adding missing grouping variables: `a b`

# dplyr_col_select() aborts when `[` implementation is broken

    Code
      dplyr_col_select(df1, 2)
    Error <vctrs_error_subscript_oob>
      Can't subset elements that don't exist.
      x Location 2 doesn't exist.
      i There are only 1 element.

---

    Code
      dplyr_col_select(df1, 1)
    Error <rlang_error>
      Can't reconstruct data frame.
      x The `[` method for class <dplyr_test_broken_operator/tbl_df/tbl/data.frame> must return a data frame.
      i It returned a <list>.

---

    Code
      dplyr_col_select(df2, 1)
    Error <rlang_error>
      Can't reconstruct data frame.
      x The `[` method for class <dplyr_test_operator_wrong_size/tbl_df/tbl/data.frame> must return a data frame with 1 column.
      i It returned a <data.frame> of 0 columns.

