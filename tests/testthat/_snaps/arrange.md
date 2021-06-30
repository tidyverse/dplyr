# arrange() gives meaningful errors

    Code
      tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)
    Error <dplyr_error>
      arrange() failed at implicit mutate() step. 
      x Can't transform a data frame with duplicate names.

---

    Code
      tibble(x = 1) %>% arrange(y)
    Error <dplyr_error>
      arrange() failed at implicit mutate() step. 
      x Problem with `mutate()` column `..1`.
      i `..1 = y`.
      x object 'y' not found

---

    Code
      tibble(x = 1) %>% arrange(rep(x, 2))
    Error <dplyr_error>
      arrange() failed at implicit mutate() step. 
      x Problem with `mutate()` column `..1`.
      i `..1 = rep(x, 2)`.
      i `..1` must be size 1, not 2.

# desc() inside arrange() checks the number of arguments (#5921)

    Code
      df <- data.frame(x = 1, y = 2)
      arrange(df, desc(x, y))
    Error <rlang_error>
      `desc()` expects exactly one argument.

