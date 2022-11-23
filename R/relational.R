rel_translate_dots <- function(dots, data) {
  map(dots, rel_translate, data)
}

rel_translate <- function(quo, data) {
  env <- quo_get_env(quo)

  do_translate <- function(expr) {
    switch(typeof(expr[[1]]),
      character = ,
      logical = ,
      integer = ,
      double = relational::expr_constant(expr),
      abort(paste0("Internal: Unknown type ", typeof(expr[[1]])))
    )
  }

  do_translate(quo_get_expr(quo))
}
