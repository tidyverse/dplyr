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
#'
#' players <- group(baseball_s, id)
#' summarise(players, g = mean(g), n = count())
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
  where <- to_sql(x$filter)
  n <- sql_select(x, "count()", where = where, n = -1L)[[1]]

  c(n, length(source_vars(x)))
}

#' @S3method head source_sqlite
head.source_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  where <- to_sql(x$filter)
  order_by <- to_sql(x$arrange)

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
  summarise <- to_sql(source$summarise)
  mutate <- to_sql(source$mutate)
  group_by <- to_sql(source$group)

  select <- c(group_by, source$select, summarise, mutate)
  if (length(select) == 0) vars <- "*"

  where <- to_sql(source$filter)
  order_by <- to_sql(source$arrange)

  sql_select(source,
    select = select,
    where = where,
    order_by = order_by,
    group_by = group_by,
    n = n)
}

do.source_sqlite <- function(source, fun, ...) {
  fun(render(source), ...)
}
