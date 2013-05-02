#' Create an sqlite data source.
#'
#' @param path path to sqlite database
#' @param table name of table in database
#' @examples
#' path <- system.file("db/baseball.sqlite3", package = "dply")
#' path <- "inst/db/baseball.sqlite3"
#' baseball_s <- sqlite_source(path, "baseball")
#' dim(baseball_s)
#' names(baseball_s)
#' head(baseball_s)
sqlite_source <- function(path, table) {
  assert_that(is.readable(path), is.string(table))
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  con <- dbConnect(dbDriver("SQLite"), dbname = path)
  if (!(table %in% dbListTables(con))) {
    stop("Table ", table, " not found in database ", path, call. = FALSE)
  }

  structure(list(con = con, table = table),
    class = c("source_sqlite", "source_sql", "source", "op"))
}
#' @S3method source_name source_sqlite
source_name.source_sqlite <- function(x) {
  x$table
}
#' @S3method source_vars source_sqlite
source_vars.source_sqlite <- function(x) {
  dbListFields(x$con, x$table)
}

# Standard data frame methods --------------------------------------------------

#' @S3method names source_sqlite
names.source_sqlite <- source_vars.source_sqlite
#' @S3method dimnames source_sqlite
dimnames.source_sqlite <- function(x) {
  list(NULL, source_vars.source_sqlite(x))
}
#' @S3method dim source_sqlite
dim.source_sqlite <- function(x) {
  n <- sql_select(x, "count()", n = -1L)[[1]]

  c(n, length(source_vars(x)))
}

#' @S3method head source_sqlite
head.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  sql_select(x, "*", limit = n)
}

#' @S3method tail source_sqlite
tail.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  df <- sql_select(x, "*", order_by = "ROWID DESC", limit = n)
  unrowname(df[rev(1:nrow(df)), , drop = FALSE])
}

#' @S3method as.data.frame source_sqlite
as.data.frame.source_sqlite <- function(x, row.names = NULL, optional = NULL,
                                        n = 1e5L) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  sql_select(x, "*", n = n)
}

# Non-standard evaluation ------------------------------------------------------

#' @importFrom stats setNames
#' @S3method subset source_sqlite
subset.source_sqlite <- function(x, subset, select, ..., n = 1e5L) {

  sql <- list()
  if (!missing(subset)) {
    env <- parent.frame()
    sql$where <- translate(x, substitute(subset), env)
  }

  if (!missing(select)) {
    select <- substitute(select)

    nm <- names(x)
    nm_env <- as.list(setNames(seq_along(nm), nm))
    cols <- nm[eval(select, nm_env, parent.frame())]

    sql$select <- cols
  } else {
    sql$select <- "*"
  }

  sql_select2(x, sql, n = n)
}

# Render sequence of ops to a data frame ---------------------------------------

render.source_sqlite <- function(source, ops, ..., n = 1e5, env = env) {
  n_vars <- length(ops$select) + length(ops$summarise) + length(ops$transform)
  if (n_vars == 0) ops$select <- "*"
  select <- var_names(source, ops$select)

  where <- vapply(ops$filter, translate, source = source, env = env,
    FUN.VALUE = character(1))

  sql_select(source, select = select, where = where, n = n)
}

var_names <- function(source, calls) {
  if (is.character(calls)) return(calls)
  vapply(calls, deparse, width.cutoff = 500L, FUN.VALUE = character(1))
}
