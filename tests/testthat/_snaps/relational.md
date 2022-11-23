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

