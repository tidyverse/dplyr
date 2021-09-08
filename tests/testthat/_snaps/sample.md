# sample_*() gives meaningful error messages

    Code
      sample_n(grp, nrow(df2) / 2, weight = y)
    Error <dplyr_error>
      too few positive probabilities
      i The error occurred in group 1: g = 1.

---

    Code
      sample_frac(grp, 1, weight = y)
    Error <dplyr_error>
      too few positive probabilities
      i The error occurred in group 1: g = 1.

---

    Code
      mtcars %>% group_by(cyl) %>% sample_n(10)
    Error <dplyr_error>
      `size` must be less than or equal to 7 (size of data), set `replace` = TRUE to use sampling with replacement.
      i The error occurred in group 2: cyl = 6.

---

    Code
      sample_n(list())
    Error <rlang_error>
      `tbl` must be a data frame, not a list.

---

    Code
      sample_frac(list())
    Error <rlang_error>
      `tbl` must be a data frame, not a list.

---

    Code
      check_weight(letters[1:2], 2)
    Error <rlang_error>
      `weight` must be a numeric, not a character vector.

---

    Code
      check_weight(-1:-2, 2)
    Error <rlang_error>
      `weight` must be a vector with all values nonnegative, not -1.

---

    Code
      check_weight(letters, 2)
    Error <rlang_error>
      `weight` must be a numeric, not a character vector.

---

    Code
      sample_n(df, 2, weight = y)
    Error <dplyr_error>
      too few positive probabilities

---

    Code
      sample_frac(df, 2)
    Error <dplyr_error>
      `size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement.

---

    Code
      sample_frac(df %>% group_by(y), 2)
    Error <dplyr_error>
      `size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement.
      i The error occurred in group 1: y = 0.

---

    Code
      sample_frac(df, 1, weight = y)
    Error <dplyr_error>
      too few positive probabilities

