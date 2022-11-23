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
    Output
      list(val = 3, alias = NULL) |>
        structure(class = c("relational_expr_constant", "relational_expr"))
    Code
      rel_translate(quo(a + 1), df)
    Output
      list(
        name = "+",
        args = list(
          list(name = "a", rel = NULL, alias = NULL) |>
            structure(class = c("relational_expr_reference", "relational_expr")),
          list(val = 1, alias = NULL) |>
            structure(class = c("relational_expr_constant", "relational_expr"))
        ),
        alias = NULL
      ) |>
        structure(class = c("relational_expr_function", "relational_expr"))
    Code
      rel_translate(quo(a < b), df)
    Output
      list(
        name = "<",
        args = list(
          list(name = "a", rel = NULL, alias = NULL) |>
            structure(class = c("relational_expr_reference", "relational_expr")),
          list(name = "b", rel = NULL, alias = NULL) |>
            structure(class = c("relational_expr_reference", "relational_expr"))
        ),
        alias = NULL
      ) |>
        structure(class = c("relational_expr_function", "relational_expr"))

