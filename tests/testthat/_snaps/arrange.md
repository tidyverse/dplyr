# arrange() gives meaningful errors

    Code
      (expect_error(tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! Can't transform a data frame with duplicate names.
    Code
      (expect_error(tibble(x = 1) %>% arrange(y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `arrange()`:
      i In argument: `..1 = y`.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tibble(x = 1) %>% arrange(rep(x, 2))))
    Output
      <error/dplyr:::mutate_error>
      Error in `arrange()`:
      i In argument: `..1 = rep(x, 2)`.
      Caused by error:
      ! `..1` must be size 1, not 2.

# arrange errors if stringi is not installed and a locale identifier is used

    Code
      locale_to_chr_proxy_collate("fr", has_stringi = FALSE)
    Condition
      Error:
      ! stringi >=1.5.3 is required to arrange in a different locale.

# arrange validates `.locale`

    Code
      arrange(df, .locale = 1)
    Condition
      Error in `arrange()`:
      ! `.locale` must be a string or `NULL`.

---

    Code
      arrange(df, .locale = c("en_US", "fr_BF"))
    Condition
      Error in `arrange()`:
      ! If `.locale` is a character vector, it must be a single string.

# arrange validates that `.locale` must be one from stringi

    Code
      arrange(df, .locale = "x")
    Condition
      Error in `arrange()`:
      ! `.locale` must be one of the locales within `stringi::stri_locale_list()`.

# desc() inside arrange() checks the number of arguments (#5921)

    Code
      df <- data.frame(x = 1, y = 2)
      (expect_error(arrange(df, desc(x, y))))
    Output
      <error/rlang_error>
      Error in `arrange()`:
      ! `desc()` must be called with exactly one argument.

