#' Create an sqlite data source.
#'
#' To see exactly what SQL is being sent to the database, you can set option
#' \code{dplyr.show_sql} to true: \code{options(dplyr.show_sql = TRUE)}
#'
#' @param path path to sqlite database
#' @param table name of table in database
#' @export
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- source_sqlite(db_path, "baseball")
#' dim(baseball_s)
#' names(baseball_s)
#' head(baseball_s)
#'
#' players <- group_by(baseball_s, id)
#' summarise(players, g = mean(g), n = count())
source_sqlite <- function(path, table) {
  assert_that(is.readable(path), is.string(table))
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  con <- dbConnect(dbDriver("SQLite"), dbname = path)
  if (!(table %in% dbListTables(con))) {
    stop("Table ", table, " not found in database ", path, call. = FALSE)
  }

  structure(list(
      con = con, path = path, table = table,
      select = NULL, filter = NULL, arrange = NULL
    ),
    class = c("source_sqlite", "source_sql", "source", "op")
  )
}

#' @S3method source_vars source_sqlite
source_vars.source_sqlite <- function(x) {
  dbListFields(x$con, x$table)
}

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame source_sqlite
as.data.frame.source_sqlite <- function(x, row.names = NULL, optional = NULL,
                                        ..., n = 1e5L) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  sql_select(x, n = n)
}

#' @S3method print source_sqlite
print.source_sqlite <- function(x, ...) {
  cat("Source:  SQLite [", x$path, "]\n", sep = "")
  cat("Table:   ", x$table, dim_desc(x), "\n", sep = "")
  if (!is.null(x$filter)) {
    cat(wrap("Filter:  ", commas(deparse_all(x$filter))), "\n")
  }
  if (!is.null(x$arrange)) {
    cat(wrap("Arrange:  ", commas(deparse_all(x$arrange))), "\n")
  }
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_sqlite
dimnames.source_sqlite <- function(x) {
  list(NULL, source_vars.source_sqlite(x))
}

#' @S3method dim source_sqlite
dim.source_sqlite <- function(x) {
  n <- sql_select(x, "count()")[[1]]

  if (is.null(x$select) || any(x$select == "*")) {
    p <- length(source_vars(x))
  } else {
    p <- length(x$select)
  }

  c(n, p)
}

#' @S3method head source_sqlite
head.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  sql_select(x, limit = n)
}

#' @S3method tail source_sqlite
tail.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  df <- sql_select(x, "*", order_by = "ROWID DESC", limit = n)
  unrowname(df[rev(1:nrow(df)), , drop = FALSE])
}
