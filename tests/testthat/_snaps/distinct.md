# distinct gives a warning when selecting an unknown column (#3140)

    Code
      df %>% distinct(aa, x)
    Error <rlang_error>
      `distinct()` must use existing variables.
      x `aa` not found in `.data`.

---

    Code
      df %>% distinct(aa, bb)
    Error <rlang_error>
      `distinct()` must use existing variables.
      x `aa` not found in `.data`.
      x `bb` not found in `.data`.

---

    Code
      df %>% distinct(.data$aa)
    Error <rlang_error>
      `distinct()` must use existing variables.
      x `aa` not found in `.data`.

---

    Code
      df %>% distinct(y = a + 1)
    Error <rlang_error>
      Problem adding computed columns in `distinct()`.
      x Problem with `mutate()` column `y`.
      x object 'a' not found
      i Input `y` is `a + 1`.

