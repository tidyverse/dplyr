rel_translate_dots <- function(dots, data) {
  map(dots, rel_translate, data)
}

rel_translate <- function(quo, data) {
  env <- quo_get_env(quo)

  do_translate <- function(expr) {
    switch(typeof(expr),
      character = ,
      logical = ,
      integer = ,
      double = relational::expr_constant(expr),
      #
      symbol = {
        if (as.character(expr) %in% names(data)) {
          relational::expr_reference(as.character(expr))
        } else {
          val <- eval_tidy(expr, env = env)
          relational::expr_constant(val)
        }
      },
      #
      language = {
        args <- map(expr[-1], do_translate)
        relational::expr_function(as.character(expr[[1]]), args)
      },
      #
      abort(paste0("Internal: Unknown type ", typeof(expr)))
    )
  }

  do_translate(quo_get_expr(quo))
}
