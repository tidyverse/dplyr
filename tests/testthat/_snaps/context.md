# give useful error messages when not applicable

    Code
      (expect_error(n()))
    Output
      <error/rlang_error>
      Error in `n()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_column()))
    Output
      <error/rlang_error>
      Error in `cur_column()`:
      ! Must only be used inside `across()`.
    Code
      (expect_error(cur_group()))
    Output
      <error/rlang_error>
      Error in `cur_group()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_group_id()))
    Output
      <error/rlang_error>
      Error in `cur_group_id()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(cur_group_rows()))
    Output
      <error/rlang_error>
      Error in `cur_group_rows()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

# group labels are correctly formatted

    Code
      group_labels_details(c(a = 1))
    Output
      [1] "`a = 1`"
    Code
      group_labels_details(c(a = 1, b = 2))
    Output
      [1] "`a = 1` and `b = 2`"

