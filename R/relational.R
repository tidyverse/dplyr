rel_try <- function(rel, fallback) {
  # return(rel)

  out <- try_fetch(rel, error = identity)
  if (inherits(out, "error")) {
    cnd_signal(warning_cnd(message = "Can't process with relational.", parent = out))
    fallback
  } else {
    out
  }
}

rel_translate_dots <- function(dots, data) {
  if (is.null(names(dots))) {
    map(dots, rel_translate, data)
  } else {
    imap(dots, rel_translate, data = data)
  }
}

rel_translate <- function(quo, data, alias = NULL) {
  env <- quo_get_env(quo)

  do_translate <- function(expr, alias = NULL) {
    # I don't understand yet how this can be a quosure
    stopifnot(!is_quosure(expr))

    switch(typeof(expr),
      character = ,
      logical = ,
      integer = ,
      double = relational::expr_constant(expr, alias = alias),
      #
      symbol = {
        if (as.character(expr) %in% names(data)) {
          relational::expr_reference(as.character(expr), alias = alias)
        } else {
          val <- eval_tidy(expr, env = env)
          relational::expr_constant(val, alias = alias)
        }
      },
      #
      language = {
        args <- map(expr[-1], do_translate)
        relational::expr_function(as.character(expr[[1]]), args, alias = alias)
      },
      #
      abort(paste0("Internal: Unknown type ", typeof(expr)))
    )
  }

  if (identical(alias, "")) {
    alias <- NULL
  }

  do_translate(quo_get_expr(quo), alias)
}
