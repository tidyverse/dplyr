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
  where <- translate_all(x$filter, x)
  n <- sql_select(x, "count()", where = where, n = -1L)[[1]]

  c(n, length(source_vars(x)))
}

#' @S3method head source_sqlite
head.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  where <- translate_all(x$filter, x)
  order_by <- translate_all(x$arrange, x)

  sql_select(x, "*", where = where, order_by = order_by, limit = n)
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

  render(x, n = n)
}

render.source_sqlite <- function(source, ..., n = 1e5) {
  args <- dots(...)
  if (length(args) > 0) {
    stop("This method does not accept additional arguments in ...",
      call. = FALSE)
  }

  # Source object enforces that we only ever have one of summarise or mutate
  summarise <- translate_all(source$summarise, source)
  mutate <- translate_all(source$mutate, source)

  select <- c(source$select, summarise, mutate)
  if (length(select) == 0) vars <- "*"

  where <- translate_all(source$filter, source)
  order_by <- translate_all(source$arrange, source)

  sql_select(source,
    select = select,
    where = where,
    order_by = order_by,
    n = n)
}

translate_all <- function(x, source) {
  if (length(x) == 0) return(NULL)

  translator <- source_translator(source)

  vapply(x, eval, env = translator, enclos = emptyenv(),
    FUN.VALUE = character(1))
}

do.source_sqlite <- function(source, fun, ...) {
  fun(render(source), ...)
}
