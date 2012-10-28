#' @examples
#' path <- system.file("db/baseball.sqlite3", package = "dply")
#' path <- "inst/db/baseball.sqlite3"
#' baseball_s <- sqlite_source(path, "baseball")
sqlite_source <- function(path, table) {
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }

  con <- dbConnect(dbDriver("SQLite"), dbname = path)

  structure(list(con = con, table = table),
    class = c("source_sqlite", "source"))
}

source_vars.source_sqlite <- function(x) {
  dbListFields(x$con, x$table)
}

source_name.source_sqlite <- function(x) {
  x$table
}

# library(RSQLite)
# data("baseball", package = "plyr")
# drv <- dbDriver("SQLite")
# con <- dbConnect(drv, dbname = "inst/db/baseball.sqlite3")
# dbWriteTable(con, "baseball", baseball, row.names = FALSE)
