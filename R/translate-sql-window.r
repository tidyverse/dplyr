uses_window_fun <- function(x, con) {
  if (is.null(x)) return(FALSE)
  if (inherits(x, "lazy_dots")) {
    calls <- unlist(lapply(x, function(x) all_calls(x$expr)))
  } else {
    calls <- all_calls(x)
  }

  win_f <- ls(envir = sql_translate_env(con)$window)
  any(calls %in% win_f)
}

#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
#' translate_window_where(list(quote(x == 1), quote(n() > 2)))
#' translate_window_where(list(quote(cumsum(x) == 10), quote(n() > 2)))
translate_window_where <- function(expr, window_funs = ls(sql_translate_env(NULL)$window)) {
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(
      expr = expr,
      comp = list()
    ))
  }

  # Other base case is an aggregation function --------------------------------
  if (is.call(expr) && as.character(expr[[1]]) %in% window_funs) {
    name <- unique_name()

    return(list(
      expr = as.name(name),
      comp = setNames(list(expr), name)
    ))
  }

  # Recursive cases: list and all other functions -----------------------------

  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, window_funs = window_funs)
    call <- unlist(lapply(args, "[[", "expr"), recursive = FALSE)
  } else {
    args <- lapply(expr[-1], translate_window_where, window_funs = window_funs)
    call <- list(as.call(c(expr[[1]], lapply(args, "[[", "expr"))))
  }

  comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)

  list(
    expr = call,
    comp = comps
  )
}
