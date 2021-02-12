# do() gives meaningful error messages

    Code
      df %>% do(head, tail)
    Error <rlang_error>
      Can only supply one unnamed argument, not 2.

---

    Code
      df %>% ungroup() %>% do(1)
    Error <rlang_error>
      Result must be a data frame, not numeric

---

    Code
      df %>% do(1)
    Error <rlang_error>
      Results 1, 2, 3 must be data frames, not numeric

---

    Code
      df %>% do("a")
    Error <rlang_error>
      Results 1, 2, 3 must be data frames, not character

---

    Code
      df %>% do(x = 1, 2)
    Error <rlang_error>
      Arguments must either be all named or all unnamed.

