# colwise select() / rename() give meaningful errors

    Code
      df <- tibble(x = 0L, y = 0.5, z = 1)
      (expect_error(df %>% rename_all()))
    Output
      <error/rlang_error>
      Error in `rename_all()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(df %>% rename_if(is_integerish)))
    Output
      <error/rlang_error>
      Error in `rename_if()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(df %>% rename_at(vars(x:y))))
    Output
      <error/rlang_error>
      Error in `rename_at()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(df %>% rename_all(list(tolower, toupper))))
    Output
      <error/rlang_error>
      Error in `rename_all()`:
      ! `.funs` must contain one renaming function, not 2.
    Code
      (expect_error(df %>% select_all(list(tolower, toupper))))
    Output
      <error/rlang_error>
      Error in `select_all()`:
      ! `.funs` must contain one renaming function, not 2.
    Code
      (expect_error(df %>% select_if(function(.x) 1)))
    Output
      <error/rlang_error>
      Error in `select_if()`:
      ! `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a <double> for column `x`.
    Code
      (expect_error(df %>% select_if(function(.x) c(TRUE, TRUE))))
    Output
      <error/rlang_error>
      Error in `select_if()`:
      ! `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a size 2 <logical> for column `x`.
    Code
      (expect_error(data.frame() %>% select_all(.funs = 42)))
    Output
      <error/rlang_error>
      Error in `select_all()`:
      ! `.funs` must be a one sided formula, a function, or a function name.

