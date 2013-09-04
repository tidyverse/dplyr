#' Create an sqlite tbl.
#'
#' You can create a sqlite tbl with a table name and a path or 
#' \code{\link{src_sqlite}} object. You need to use \code{src_sqlite} 
#' if you're working with multiple tables from the same database so that they
#' use the same connection, and so can perform joins etc.
#'
#' To see exactly what SQL is being sent to the database, you can set option
#' \code{dplyr.show_sql} to true: \code{options(dplyr.show_sql = TRUE).}
#' If you're wondering why a particularly query is slow, it can be helpful
#' to see the query plan. You can do this by setting
#' \code{options(dplyr.explain_sql = TRUE)}. The output of SQLite's query
#' plans is relatively easy to make sense of and is explained at
#' \url{http://www.sqlite.org/eqp.html}. You may also find the explanation of
#' how SQL indices works to be helpful:
#' \url{http://www.sqlite.org/queryplanner.html}.
#'
#' @param path,src either path to sqlite database, or \code{src_sqlite} object
#' @param table name of table in database. This can either be a string 
#'   representing a table name, or an \code{\link{sql}} string, representing
#'   a select query or compound join.
#' @param ... other arguments ignored, but needed for compatibility with 
#'   generic.
#' @export
#' @examples
#' # You can create from a path and a table name
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- tbl_sqlite(db_path, "baseball")
#' 
#' # Or (better) from a sqlite src and a table name
#' db <- src_sqlite(db_path)
#' baseball_s <- tbl(db, "baseball")
#' 
#' dim(baseball_s)
#' names(baseball_s)
#' head(baseball_s)
#'
#' players <- group_by(baseball_s, id)
#' summarise(players, g = mean(g), n = count())
tbl_sqlite <- function(path, table) {
  src <- src_sqlite(path)
  tbl(src, table)
}

#' @method tbl src_sqlite
#' @export
#' @rdname tbl_sqlite
tbl.src_sqlite <- function(src, table, ...) {
  if (!is.sql(table) && !has_table(src, table)) {
    stop("Table ", table, " not found in database ", path, call. = FALSE)
  }
  
  tbl_sql(c("sqlite_table", "sqlite"), src = src, table = table)
}

#' @S3method tbl_vars tbl_sqlite
tbl_vars.tbl_sqlite <- function(x) {
  names(sql_select(x, x$select %||% "*", where = sql("1 == 0"), 
    show = FALSE, explain = FALSE))
}

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame tbl_sqlite
as.data.frame.tbl_sqlite <- function(x, row.names = NULL, optional = NULL,
                                        ..., n = 1e5L) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  sql_select(x, n = n)
}

#' @S3method print tbl_sqlite
print.tbl_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$src$path, "]\n", sep = "")
  
  cat(wrap("From: ", gsub("\n", " ", x$table), " ", dim_desc(x)))
  cat("\n")
  if (!is.null(x$filter)) {
    cat(wrap("Filter: ", commas(deparse_all(x$filter))), "\n")
  }
  if (!is.null(x$arrange)) {
    cat(wrap("Arrange: ", commas(deparse_all(x$arrange))), "\n")
  }
  
  if (is.sql(x$table)) return()
  
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames tbl_sqlite
dimnames.tbl_sqlite <- function(x) {
  list(NULL, tbl_vars.tbl_sqlite(x))
}

#' @S3method dim tbl_sqlite
dim.tbl_sqlite <- function(x) {
  if (is.sql(x$table)) {
    n <- "??"
  } else {
    n <- sql_select(x, "count()", show = FALSE, explain = FALSE)[[1]]
  }
  
  if (is.null(x$select) || any(x$select == "*")) {
    p <- length(tbl_vars(x))
  } else {
    p <- length(x$select)
  }

  c(n, p)
}

#' @S3method head tbl_sqlite
head.tbl_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  sql_select(x, limit = n)
}

#' @S3method tail tbl_sqlite
tail.tbl_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  df <- sql_select(x, "*", order_by = "ROWID DESC", limit = n)
  unrowname(df[rev(1:nrow(df)), , drop = FALSE])
}
