#' @export
#' @rdname sql_build
sql_optimise <- function(x, con = NULL, ...) {
  UseMethod("sql_optimise")
}

#' @export
sql_optimise.sql <- function(x, con = NULL, ...) {
  # Can't optimise raw SQL
  x
}

#' @export
sql_optimise.query <- function(x, con = NULL, ...) {
  # Default to no optimisation
  x
}

#' @export
sql_optimise.select_query <- function(x, con = NULL, ...) {
  if (!inherits(x$from, "select_query")) {
    return(x)
  }

  from <- sql_optimise(x$from)

  # If all outer clauses are exeucted after the inner clauses, we
  # can drop them down a level
  outer <- select_query_clauses(x)
  inner <- select_query_clauses(from)

  if (length(outer) > 0 &&
      length(inner) > 0 &&
      min(outer) > max(inner)) {
    from[as.character(outer)] <- x[as.character(outer)]
    from
  } else {
    x
  }
}


# Helpers for testing -----------------------------------------------------

#' @export
sql_optimise.tbl_sql <- function(x, con = NULL, ...) {
  if (is.null(con)) {
    con <- con_acquire(x$src)
    on.exit(con_release(x$src, con), add = TRUE)
  }
  sql_optimise(sql_build(x$ops, con, ...), con = con, ...)
}

#' @export
sql_optimise.tbl_lazy <- function(x, con = NULL, ...) {
  sql_optimise(sql_build(x$ops, con = NULL, ...), con = NULL, ...)
}
