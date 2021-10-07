# distinct gives a warning when selecting an unknown column (#3140)

    Code
      df <- tibble(g = c(1, 2), x = c(1, 2))
      (expect_error(df %>% distinct(aa, x)))
    Output
      <error/rlang_error>
      Error in `distinct_prepare()`: `distinct()` must use existing variables.
      x `aa` not found in `.data`.
    Code
      (expect_error(df %>% distinct(aa, bb)))
    Output
      <error/rlang_error>
      Error in `distinct_prepare()`: `distinct()` must use existing variables.
      x `aa` not found in `.data`.
      x `bb` not found in `.data`.
    Code
      (expect_error(df %>% distinct(.data$aa)))
    Output
      <error/rlang_error>
      Error in `distinct_prepare()`: `distinct()` must use existing variables.
      x `aa` not found in `.data`.
    Code
      (expect_error(df %>% distinct(y = a + 1)))
    Output
      <error/rlang_error>
      Error: 
        Problem adding computed columns in `distinct()`.
        x Problem with `mutate()` column `y`.
        i `y = a + 1`.
        x object 'a' not found
      Caused by error: 
        Problem with `mutate()` column `y`.
        i `y = a + 1`.
        x object 'a' not found
      Caused by error: 
        object 'a' not found

