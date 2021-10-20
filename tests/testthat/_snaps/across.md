# across() gives meaningful messages

    Code
      (expect_error(tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: 
        Problem while computing `res = across(where(is.numeric), 42)`.
      Caused by error in `across_setup()`: 
        Problem with `across()` input `.fns`.
        i `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(across()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `across()` must only be used inside dplyr verbs.
    Code
      (expect_error(c_across()))
    Output
      <error/rlang_error>
      Error in `context_peek()`: `c_across()` must only be used inside dplyr verbs.

# if_any() and if_all() aborts when predicate mistakingly used in .cols= (#5732)

    Code
      (expect_error(filter(df, if_any(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`: 
        Problem while expanding `..1 = if_any(~.x > 5)`.
      Caused by error in `across_setup()`: 
        Predicate used in lieu of column selection.
        i You most likely meant: `if_any(everything(), ~.x > 5)`.
        i The first argument `.cols` selects a set of columns.
        i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, if_all(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`: 
        Problem while expanding `..1 = if_all(~.x > 5)`.
      Caused by error in `across_setup()`: 
        Predicate used in lieu of column selection.
        i You most likely meant: `if_all(everything(), ~.x > 5)`.
        i The first argument `.cols` selects a set of columns.
        i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, !if_any(~ .x > 5))))
    Output
      <error/dplyr_error>
      Error in `filter()`: 
        Problem while computing `..1 = !if_any(~.x > 5)`.
      Caused by error in `across_setup()`: 
        Predicate used in lieu of column selection.
        i You most likely meant: `if_any(everything(), ~.x > 5)`.
        i The first argument `.cols` selects a set of columns.
        i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, !if_all(~ .x > 5))))
    Output
      <error/dplyr_error>
      Error in `filter()`: 
        Problem while computing `..1 = !if_all(~.x > 5)`.
      Caused by error in `across_setup()`: 
        Predicate used in lieu of column selection.
        i You most likely meant: `if_all(everything(), ~.x > 5)`.
        i The first argument `.cols` selects a set of columns.
        i The second argument `.fns` operates on each selected columns.

