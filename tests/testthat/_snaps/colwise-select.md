# colwise select() / rename() give meaningful errors

    Code
      df <- tibble(x = 0L, y = 0.5, z = 1)
      (expect_error(rename_all(df)))
    Output
      <error/rlang_error>
      Error in `rename_all()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(rename_if(df, is_integerish)))
    Output
      <error/rlang_error>
      Error in `rename_if()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(rename_at(df, vars(x:y))))
    Output
      <error/rlang_error>
      Error in `rename_at()`:
      ! `.funs` must specify a renaming function.
    Code
      (expect_error(rename_all(df, list(tolower, toupper))))
    Output
      <error/rlang_error>
      Error in `rename_all()`:
      ! `.funs` must contain one renaming function, not 2.
    Code
      (expect_error(select_all(df, list(tolower, toupper))))
    Output
      <error/rlang_error>
      Error in `select_all()`:
      ! `.funs` must contain one renaming function, not 2.
    Code
      (expect_error(select_if(df, function(.x) 1)))
    Output
      <error/rlang_error>
      Error in `select_if()`:
      ! `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a <double> for column `x`.
    Code
      (expect_error(select_if(df, function(.x) c(TRUE, TRUE))))
    Output
      <error/rlang_error>
      Error in `select_if()`:
      ! `.p` is invalid.
      x `.p` should return a single logical.
      i `.p` returns a size 2 <logical> for column `x`.
    Code
      (expect_error(select_all(data.frame(), .funs = 42)))
    Output
      <error/rlang_error>
      Error in `select_all()`:
      ! `.funs` must be a one sided formula, a function, or a function name.

