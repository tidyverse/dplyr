# colwise filter() give meaningful errors

    Code
      filter_if(mtcars, is_character, all_vars(. > 0))
    Error <rlang_error>
      `.predicate` has no matching columns.

---

    Code
      filter_all(mtcars, list(~. > 0))
    Error <rlang_error>
      `.vars_predicate` must be a function or a call to `all_vars()` or `any_vars()`, not a list.

