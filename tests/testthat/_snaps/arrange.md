# arrange() gives meaningful errors

    Code
      (expect_error(tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! Problem with the implicit `transmute()` step.
      Caused by error in `transmute()`:
      ! Can't transform a data frame with duplicate names.
    Code
      (expect_error(tibble(x = 1) %>% arrange(y)))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! Problem with the implicit `transmute()` step.
      x Problem while computing `..1 = y`.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tibble(x = 1) %>% arrange(rep(x, 2))))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! Problem with the implicit `transmute()` step.
      x Problem while computing `..1 = rep(x, 2)`.
      x `..1` must be size 1, not 2.

# desc() inside arrange() checks the number of arguments (#5921)

    Code
      df <- data.frame(x = 1, y = 2)
      (expect_error(arrange(df, desc(x, y))))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! `desc()` must be called with exactly one argument.

