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

# arrange errors if stringi is not installed and a locale identifier is used

    Code
      (expect_error(locale_to_chr_proxy_collate("fr", has_stringi = FALSE)))
    Output
      <error/rlang_error>
      Error in `locale_to_chr_proxy_collate()`: 
      stringi >= 1.5.3 is required to arrange in a different locale.

# arrange validates `.locale`

    Code
      (expect_error(arrange(df, .locale = 1)))
    Output
      <error/rlang_error>
      Error in `locale_to_chr_proxy_collate()`: 
      `.locale` must be a string.
    Code
      (expect_error(arrange(df, .locale = c("en_US", "fr_BF"))))
    Output
      <error/rlang_error>
      Error in `locale_to_chr_proxy_collate()`: 
      If `.locale` is a character vector, it must be a single string.
    Code
      (expect_error(arrange(df, .locale = "x")))
    Output
      <error/rlang_error>
      Error in `locale_to_chr_proxy_collate()`: 
      `.locale` must be one of the locales within `stringi::stri_locale_list()`.

# desc() inside arrange() checks the number of arguments (#5921)

    Code
      df <- data.frame(x = 1, y = 2)
      arrange(df, desc(x, y))
    Error <rlang_error>
      `desc()` expects exactly one argument.

