# rows_delete()

    Code
      res <- rows_delete(data, tibble(a = 2:3, b = "b"), by = "a")
    Message <dplyr_message_delete_extra_cols>
      Ignoring extra columns: b

# rows_*() errors

    Code
      rows_insert(data, tibble(a = 3, b = "z"))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Error <rlang_error>
      Attempting to insert duplicate rows.

---

    Code
      rows_insert(data[c(1, 1), ], tibble(a = 3))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Error <rlang_error>
      `x` key values are not unique.

---

    Code
      rows_insert(data, tibble(a = 4, b = "z"), by = "e")
    Error <rlang_error>
      All `by` columns must exist in `x`.

---

    Code
      rows_insert(data, tibble(d = 4))
    Message <dplyr_message_matching_by>
      Matching, by = "d"
    Error <rlang_error>
      All columns in `y` must exist in `x`.

---

    Code
      rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
    Error <rlang_error>
      Attempting to update missing rows.

---

    Code
      rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
    Error <rlang_error>
      Attempting to patch missing rows.

---

    Code
      rows_delete(data, tibble(a = 2:4))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Error <rlang_error>
      Attempting to delete missing rows.

---

    Code
      rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b"))
    Error <rlang_error>
      Attempting to delete missing rows.

---

    Code
      rows_delete(data, tibble(a = 2:3))
    Message <dplyr_message_matching_by>
      Matching, by = "a"
    Output
      # A tibble: 1 x 3
            a b         c
        <int> <chr> <dbl>
      1     1 a       0.5

---

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

