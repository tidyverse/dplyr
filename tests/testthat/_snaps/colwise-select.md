# colwise select() / rename() give meaningful errors

    Code
      df %>% rename_all()
    Error <rlang_error>
      `.funs` must specify a renaming function.

---

    Code
      df %>% rename_if(is_integerish)
    Error <rlang_error>
      `.funs` must specify a renaming function.

---

    Code
      df %>% rename_at(vars(x:y))
    Error <rlang_error>
      `.funs` must specify a renaming function.

---

    Code
      df %>% rename_all(list(tolower, toupper))
    Error <rlang_error>
      `.funs` must contain one renaming function, not 2.

---

    Code
      df %>% select_all(list(tolower, toupper))
    Error <rlang_error>
      `.funs` must contain one renaming function, not 2.

---

    Code
      df %>% select_if(function(.x) 1)
    Error <rlang_error>
      `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a <double> for column `x`.

---

    Code
      df %>% select_if(function(.x) c(TRUE, TRUE))
    Error <rlang_error>
      `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a size 2 <logical> for column `x`.

