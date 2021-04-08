# across() gives meaningful messages

    Code
      tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))
    Error <dplyr_error>
      Problem with `summarise()` column `res`.
      x Problem with `across()` input `.fns`.
      i Input `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
      i Input `res` is `across(where(is.numeric), 42)`.

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

