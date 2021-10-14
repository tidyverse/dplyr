# arrange() gives meaningful errors

    Code
      (expect_error(tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)))
    Output
      <error/dplyr_error>
      Error in `arrange()`: arrange() failed at implicit mutate() step. 
      x Can't transform a data frame with duplicate names.
    Code
      (expect_error(tibble(x = 1) %>% arrange(y)))
    Output
      <error/dplyr_error>
      Error in `arrange()`: arrange() failed at implicit mutate() step. 
      x Problem while computing `..1 = y`.
    Code
      (expect_error(tibble(x = 1) %>% arrange(rep(x, 2))))
    Output
      <error/dplyr_error>
      Error in `arrange()`: arrange() failed at implicit mutate() step. 
      x Problem while computing `..1 = rep(x, 2)`.
      i `..1` must be size 1, not 2.

# desc() inside arrange() checks the number of arguments (#5921)

    Code
      df <- data.frame(x = 1, y = 2)
      (expect_error(arrange(df, desc(x, y))))
    Output
      <error/rlang_error>
      Error in `FUN()`: `desc()` expects exactly one argument.

