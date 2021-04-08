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
      * Problem with `mutate()` column `..1`.
      x object 'y' not found
      i Input `..1` is `y`.

---

    Code
      tibble(x = 1) %>% arrange(rep(x, 2))
    Error <dplyr_error>
      arrange() failed at implicit mutate() step. 
      * Problem with `mutate()` column `..1`.
      i Input `..1` must be size 1, not 2.
      i Input `..1` is `rep(x, 2)`.

