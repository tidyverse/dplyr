
#' @export
sql_subquery.bigquery <- function (con, sql, name = unique_name(), ...)
{
  if (dplyr::is.ident(sql)) return(sql)

  sql_subquery <- dplyr::build_sql("(", sql, ") AS ", dplyr::ident(name), con = con)

  # build_sql removes "vars" from SQL but we do need to have them
  # for subsequent use inside sql_join.bigquery.
  # so we put them back
  attr(sql_subquery, "vars") <- attr(sql, "vars")
  (sql_subquery)
}

#' @export
sql_join.bigquery <- function(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
                 left = sql("LEFT"),
                 inner = sql("INNER"),
                 right = sql("RIGHT"),
                 full = sql("FULL"),
                 stop("Unknown join type:", type, call. = FALSE)
  )

  by <- common_by(by, x, y)
  using <- all(by$x == by$y)

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select)
  uniques <- unique_names(x_names, y_names, by$x[by$x == by$y], x_suffix="_x", y_suffix="_y")

  if (is.null(uniques)) {
    sel_vars <- c(x_names, y_names)
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])

    sel_vars <- unique(c(uniques$x, uniques$y))
  }

  name_left <- unique_name()
  name_right <- unique_name()

  on <- sql_vector(paste0( sql_escape_ident(con, paste0(name_left,".",by$x)),
                           " = ",
                           sql_escape_ident(con, paste0(name_right,".",by$y))
  ),
  collapse = " AND ", parens = TRUE, con = con)
  cond <- build_sql("ON ", on, con = con)

  join_vars <- paste0(name_left,".",by$x)
  names(join_vars) <- by$x
  non_join_vars <- setdiff(setdiff(sel_vars, by$x), by$y)

  sql_var_list <- sql_vector( c(sql_escape_ident(con, join_vars),
                                sql_escape_ident(con, non_join_vars)),
                              collapse=",", parens = FALSE, con = con )

  sel_vars <- union(by$x, non_join_vars)

  from <- build_sql(
    'SELECT ', sql_var_list,' FROM ',
    sql_subquery(con, x$query$sql, name=name_left), "\n\n",
    join, " JOIN EACH\n\n" ,
    sql_subquery(con, y$query$sql, name=name_right), "\n\n",
    cond, con = con
  )
  attr(from, "vars") <- lapply(sel_vars, as.name)

  from
}


