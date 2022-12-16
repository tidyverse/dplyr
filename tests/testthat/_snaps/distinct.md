# distinct errors when selecting an unknown column (#3140)

    Code
      df <- tibble(g = c(1, 2), x = c(1, 2))
      (expect_error(df %>% distinct(aa, x)))
    Output
      <error/rlang_error>
      Error in `distinct()`:
      ! Must use existing variables.
      x `aa` not found in `.data`.
    Code
      (expect_error(df %>% distinct(aa, bb)))
    Output
      <error/rlang_error>
      Error in `distinct()`:
      ! Must use existing variables.
      x `aa` not found in `.data`.
      x `bb` not found in `.data`.
    Code
      (expect_error(df %>% distinct(.data$aa)))
    Output
      <error/rlang_error>
      Error in `distinct()`:
      ! Must use existing variables.
      x `aa` not found in `.data`.
    Code
      (expect_error(df %>% distinct(y = a + 1)))
    Output
      <error/dplyr:::mutate_error>
      Error in `distinct()`:
      i In argument: `y = a + 1`.
      Caused by error:
      ! object 'a' not found

