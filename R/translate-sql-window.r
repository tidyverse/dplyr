# batting <- tbl(lahman_postgres(), "Batting")
# players <- group_by(batting, teamID)
# translate_window_where(quote(1), players)
# translate_window_where(quote(x), players)
# translate_window_where(quote(x == 1), players)
# translate_window_where(quote(x == 1 && y == 2), players)
# translate_window_where(quote(n() > 10), players)
# translate_window_where(quote(rank() > cumsum(AB)), players)
# translate_window_where(list(quote(x == 1), quote(n() > 2)), players)
translate_window_where <- function(expr, tbl, con = NULL) {
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(
      expr = expr,
      comp = list()
    ))
  }

  # Other base case is an aggregation function --------------------------------
  variant <- src_translate_env(tbl)
  agg_f <- ls(envir = variant$window)

  if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
    name <- unique_name()
    sql <- translate_sql_q(list(expr), tbl, env = NULL, window = TRUE)

    return(list(
      expr = as.name(name),
      comp = setNames(list(sql), name)
    ))
  }

  # Recursive cases: list and all other functions -----------------------------

  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, tbl = tbl, con = con)

    env <- sql_env(call, variant, con = con)
    sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)
  } else {
    args <- lapply(expr[-1], translate_window_where, tbl = tbl, con = con)

    call <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))
    env <- sql_env(call, variant, con = con)
    sql <- eval(call, envir = env)
  }

  comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)

  list(
    expr = sql,
    comp = comps
  )
}
