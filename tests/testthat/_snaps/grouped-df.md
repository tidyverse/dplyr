# validate_grouped_df() gives useful errors

    Code
      validate_grouped_df(df1)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      validate_grouped_df(df2)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      validate_grouped_df(df3)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      validate_grouped_df(df4)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      validate_grouped_df(df5)
    Error <rlang_error>
      Corrupt `grouped_df` using old (< 0.8.0) format.
      i Strip off old grouping with `ungroup()`.

---

    Code
      validate_grouped_df(df6, check_bounds = TRUE)
    Error <rlang_error>
      out of bounds indices.

---

    Code
      validate_grouped_df(df7, check_bounds = TRUE)
    Error <rlang_error>
      out of bounds indices.

---

    Code
      validate_grouped_df(df8, check_bounds = TRUE)
    Error <rlang_error>
      out of bounds indices.

---

    Code
      validate_grouped_df(df10)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      validate_grouped_df(df11)
    Error <rlang_error>
      The `groups` attribute is not a data frame with its last column called `.rows`.

---

    Code
      new_grouped_df(tibble(x = 1:10), tibble(other = list(1:2)))
    Error <rlang_error>
      `new_grouped_df()` incompatible argument.
      i `groups` should be a data frame, and its last column be called `.rows`.

---

    Code
      new_grouped_df(10)
    Error <rlang_error>
      `new_grouped_df()` incompatible argument.
      x `x` is not a data frame.

# helper gives meaningful error messages

    Code
      grouped_df(data.frame(x = 1), "y", FALSE)
    Error <rlang_error>
      `vars` missing from `data`: `y`.

---

    Code
      grouped_df(data.frame(x = 1), 1)
    Error <rlang_error>
      `vars` must be a character vector.

