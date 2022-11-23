# translation

    Code
      rel_translate(quo(42))
    Output
      list(val = 42, alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))
    Code
      rel_translate(quo(42L))
    Output
      list(val = 42L, alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))
    Code
      rel_translate(quo("fortytwo"))
    Output
      list(val = "fortytwo", alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))
    Code
      rel_translate(quo(TRUE))
    Output
      list(val = TRUE, alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))
    Code
      rel_translate(quo(a), df)
    Output
      list(name = "a", rel = NULL, alias = NULL) |>
        structure(class = c("relational_expr_reference", "relational_expr"))
    Code
      rel_translate(quo(global), df)
    Condition
      Warning:
      Passing an environment as data mask is deprecated.
      Please use `new_data_mask()` to transform your environment to a mask.
      
        env <- env(foo = "bar")
      
        # Bad:
        as_data_mask(env)
        eval_tidy(expr, env)
      
        # Good:
        mask <- new_data_mask(env)
        eval_tidy(expr, mask)
      This warning is displayed once per session.
    Output
      list(val = 3, alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))

