
as_fun_list <- function(.funs, .env, ..., .caller, .caller_arg = "...") {
  args <- list2(...)

  if (is_fun_list(.funs)) {
    if (!is_empty(args)) {
      .funs[] <- map(.funs, call_modify, !!!args)
    }
    return(.funs)
  }

  if (is_list(.funs) && length(.funs) > 1) {
    .funs <- auto_name_formulas(.funs)
  }

  if (!is_character(.funs) && !is_list(.funs)) {
    .funs <- list(.funs)
  }

  if(is_character(.funs) && is_null(names(.funs)) && length(.funs) != 1L) {
    names(.funs) <- .funs
  }

  funs <- map(.funs, function(.x){
    if (is_formula(.x)) {
      if (is_quosure(.x)) {
        what <- paste0(
          "dplyr::", .caller, "(", .caller_arg, " = ",
          "'can\\'t contain quosures.')"
        )

        lifecycle::deprecate_warn(
          "0.8.3", what,
          details = "Please use a one-sided formula, a function, or a function name.",
          env = .env
        )
        .x <- new_formula(NULL, quo_squash(.x), quo_get_env(.x))
      }
      .x <- as_inlined_function(.x, env = .env)
    } else {
      if (is_character(.x)) {
        .x <- get(.x, .env, mode = "function")
      } else if (!is_function(.x)) {
        abort("expecting a one sided formula, a function, or a function name.")
      }
      if (length(args)) {
        .x <- new_quosure(
          call2(.x, quote(.), !!!args),
          env = .env
        )
      }
    }
    .x
  })
  attr(funs, "have_name") <- any(names2(funs) != "")
  funs
}

auto_name_formulas <- function(funs) {
  where <- !have_name(funs) & map_lgl(funs, function(x) is_bare_formula(x) && is_call(f_rhs(x)))
  names(funs)[where] <- map_chr(funs[where], function(x) as_label(f_rhs(x)[[1]]))
  funs
}

as_fun <- function(.x, .env, .args) {
  quo <- as_quosure(.x, .env)

  # For legacy reasons, we support strings. Those are enclosed in the
  # empty environment and need to be switched to the caller environment.
  quo <- quo_set_env(quo, fun_env(quo, .env))

  expr <- quo_get_expr(quo)

  if (is_call(expr, c("function", "~"))) {
    top_level <- as_string(expr[[1]])
    bad_args(quo_text(expr), "must be a function name (quoted or unquoted) or an unquoted call, not `{top_level}`.")
  }

  if (is_call(expr) && !is_call(expr, c("::", ":::"))) {
    expr <- call_modify(expr, !!!.args)
  } else {
    expr <- call2(expr, quote(.), !!!.args)
  }

  set_expr(quo, expr)
}

quo_as_function <- function(quo) {
  new_function(exprs(. = ), quo_get_expr(quo), quo_get_env(quo))
}

fun_env <- function(quo, default_env) {
  env <- quo_get_env(quo)
  if (is_null(env) || identical(env, empty_env())) {
    default_env
  } else {
    env
  }
}

is_fun_list <- function(x) {
  inherits(x, "fun_list")
}
#' @export
`[.fun_list` <- function(x, i) {
  structure(NextMethod(),
    class = "fun_list",
    has_names = attr(x, "has_names")
  )
}
#' @export
print.fun_list <- function(x, ..., width = getOption("width")) {
  cat("<fun_calls>\n")
  names <- format(names(x))

  code <- map_chr(x, function(x) deparse_trunc(quo_get_expr(x), width - 2 - nchar(names[1])))

  cat(paste0("$ ", names, ": ", code, collapse = "\n"))
  cat("\n")
  invisible(x)
}
