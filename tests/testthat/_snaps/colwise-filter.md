# colwise filter() give meaningful errors

    Code
      (expect_error(filter_if(mtcars, is_character, all_vars(. > 0))))
    Output
      <error/rlang_error>
      Error in `apply_filter_syms()`: `.predicate` has no matching columns.
    Code
      (expect_error(filter_all(mtcars, list(~ . > 0))))
    Output
      <error/rlang_error>
      Error in `apply_filter_syms()`: `.vars_predicate` must be a function or a call to `all_vars()` or `any_vars()`, not a list.

