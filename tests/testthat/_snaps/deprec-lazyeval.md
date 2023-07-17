# mutate_each() and mutate_each_() are deprecated (#6869)

    Code
      mutate_each(df, list(~ .x + 1L))
    Condition
      Warning:
      `mutate_each()` was deprecated in dplyr 0.7.0.
      i Please use `across()` instead.
    Output
      # A tibble: 2 x 2
            x     y
        <int> <int>
      1     2     4
      2     3     5

---

    Code
      mutate_each_(df, list(~ .x + 1L), c("x", "y"))
    Condition
      Warning:
      `mutate_each_()` was deprecated in dplyr 0.7.0.
      i Please use `across()` instead.
    Output
      # A tibble: 2 x 2
            x     y
        <int> <int>
      1     2     4
      2     3     5

# summarise_each() and summarise_each_() are deprecated (#6869)

    Code
      summarise_each(df, list(mean))
    Condition
      Warning:
      `summarise_each()` was deprecated in dplyr 0.7.0.
      i Please use `across()` instead.
    Output
      # A tibble: 1 x 2
            x     y
        <dbl> <dbl>
      1   1.5   3.5

---

    Code
      summarise_each_(df, list(mean), c("x", "y"))
    Condition
      Warning:
      `summarise_each_()` was deprecated in dplyr 0.7.0.
      i Please use `across()` instead.
    Output
      # A tibble: 1 x 2
            x     y
        <dbl> <dbl>
      1   1.5   3.5

