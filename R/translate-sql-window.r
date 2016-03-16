uses_window_fun <- function(x, con) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    calls <- unlist(lapply(x, all_calls))
  } else {
    calls <- all_calls(x)
  }

  win_f <- ls(envir = sql_translate_env(con)$window)
  any(calls %in% win_f)
}


# # window functions in WHERE need to be performed in subquery
# where <- translate_window_where(x$where, x, con = x$src$con)
# base_query <- update(x,
#   group_by = NULL,
#   where = NULL,
#   select = c(x$select, where$comp))$query
#
# from_sql <- sql_subquery(x$src$con, base_query$sql, unique_name())
# where_sql <- translate(where$expr)



# batting <- tbl(lahman_postgres(), "Batting")
# players <- group_by(batting, teamID)
# translate_window_where(quote(1), players)
# translate_window_where(quote(x), players)
# translate_window_where(quote(x == 1), players)
# translate_window_where(quote(x == 1 && y == 2), players)
# translate_window_where(quote(n() > 10), players)
# translate_window_where(quote(rank() > cumsum(AB)), players)
# translate_window_where(list(quote(x == 1), quote(n() > 2)), players)
translate_window_where <- function(expr, vars, con = NULL) {
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(
      expr = expr,
      comp = list()
    ))
  }

  # Other base case is an aggregation function --------------------------------
  variant <- sql_translate_env(con)
  agg_f <- ls(envir = variant$window)

  if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
    name <- unique_name()
    sql <- translate_sql_(list(expr), vars = vars)

    return(list(
      expr = as.name(name),
      comp = setNames(list(sql), name)
    ))
  }

  # Recursive cases: list and all other functions -----------------------------

  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, vars = vars, con = con)

    env <- sql_env(call, variant, con = con)
    sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)
  } else {
    args <- lapply(expr[-1], translate_window_where, vars = vars, con = con)

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
