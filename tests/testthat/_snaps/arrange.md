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
      Problem with `mutate()` column `..1`.
      i `..1 = y`.
      x object 'y' not found

---

    Code
      tibble(x = 1) %>% arrange(rep(x, 2))
    Error <dplyr_error>
      arrange() failed at implicit mutate() step. 
      Problem with `mutate()` column `..1`.
      i `..1 = rep(x, 2)`.
      i `..1` must be size 1, not 2.

# arrange validates `.locale`

    Code
      (expect_error(arrange(df, .locale = 1)))
    Output
      <error/rlang_error>
      `locale` must be a string, a function, or `NULL`.
    Code
      (expect_error(arrange(df, .locale = c("en_US", "fr_BF"))))
    Output
      <error/rlang_error>
      If `locale` is a character vector, it must be a single string.
    Code
      (expect_error(arrange(df, .locale = "x")))
    Output
      <error/rlang_error>
      `locale` must be one of the locales within `stringi::stri_locale_list()`.

