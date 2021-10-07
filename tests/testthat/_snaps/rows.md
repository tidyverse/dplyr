# rows_delete()

    Code
      res <- rows_delete(data, tibble(a = 2:3, b = "b"), by = "a")
    Message <dplyr_message_delete_extra_cols>
      Ignoring extra columns: b

# rows_*() errors

    Code
      data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
      (expect_error(rows_insert(data, tibble(a = 3, b = "z"))))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Output
      <error/rlang_error>
      Error in `rows_insert.data.frame()`: Attempting to insert duplicate rows.
    Code
      (expect_error(rows_insert(data[c(1, 1), ], tibble(a = 3))))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Output
      <error/rlang_error>
      Error in `rows_check_key_df()`: `x` key values are not unique.
    Code
      (expect_error(rows_insert(data, tibble(a = 4, b = "z"), by = "e")))
    Output
      <error/rlang_error>
      Error in `rows_check_key_df()`: All `by` columns must exist in `x`.
    Code
      (expect_error(rows_insert(data, tibble(d = 4))))
    Message <dplyr_message_matching_by>
      Matching, by = "d"
    Output
      <error/rlang_error>
      Error in `rows_check_key()`: All columns in `y` must exist in `x`.
    Code
      (expect_error(rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_update.data.frame()`: Attempting to update missing rows.
    Code
      (expect_error(rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_patch.data.frame()`: Attempting to patch missing rows.
    Code
      (expect_error(rows_delete(data, tibble(a = 2:4))))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Output
      <error/rlang_error>
      Error in `rows_delete.data.frame()`: Attempting to delete missing rows.
    Code
      (expect_error(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_delete.data.frame()`: Attempting to delete missing rows.
    Code
      rows_delete(data, tibble(a = 2:3))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Output
      # A tibble: 1 x 3
            a b         c
        <int> <chr> <dbl>
      1     1 a       0.5
    Code
      rows_delete(data, tibble(a = 2:3, b = "b"))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Message <dplyr_message_delete_extra_cols>
      Ignoring extra columns: b
    Output
      # A tibble: 1 x 3
            a b         c
        <int> <chr> <dbl>
      1     1 a       0.5

