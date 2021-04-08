# across() gives meaningful messages

    Code
      tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))
    Error <dplyr_error>
      Problem with `summarise()` column `res`.
      i `res = across(where(is.numeric), 42)`.
      x Problem with `across()` input `.fns`.
      i `.fns` must be NULL, a function, a formula, or a list of functions/formulas.

---

    Code
      across()
    Error <rlang_error>
      `across()` must only be used inside dplyr verbs.

---

    Code
      c_across()
    Error <rlang_error>
      `c_across()` must only be used inside dplyr verbs.

