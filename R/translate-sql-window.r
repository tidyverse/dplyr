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

common_window_funs <- ls(sql_translate_env(NULL)$window)

#' @noRd
#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
translate_window_where <- function(expr, window_funs = common_window_funs) {
  if (is.atomic(expr) || is.name(expr)) {
    window_where(expr, list())
  } else if (is.call(expr)) {
    if (as.character(expr[[1]]) %in% window_funs) {
      name <- unique_name()
      window_where(as.name(name), setNames(list(expr), name))
    } else {
      args <- lapply(expr[-1], translate_window_where, window_funs = window_funs)
      expr <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))

      window_where(
        expr = expr,
        comp = unlist(lapply(args, "[[", "comp"), recursive = FALSE)
      )
    }
  } else {
    stop("Unknown type: ", typeof(expr))
  }
}


#' @noRd
#' @examples
#' translate_window_where_all(list(quote(x == 1), quote(n() > 2)))
#' translate_window_where_all(list(quote(cumsum(x) == 10), quote(n() > 2)))
translate_window_where_all <- function(x, window_funs = common_window_funs) {
  out <- lapply(x, translate_window_where, window_funs = window_funs)

  list(
    expr = unlist(lapply(out, "[[", "expr"), recursive = FALSE),
    comp = unlist(lapply(out, "[[", "comp"), recursive = FALSE)
  )
}

window_where <- function(expr, comp) {
  stopifnot(is.call(expr) || is.name(expr) || is.atomic(expr))
  stopifnot(is.list(comp))

  list(
    expr = expr,
    comp = comp
  )
}
