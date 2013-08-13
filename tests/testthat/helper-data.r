library(data.table)
library(RSQLite)

make_sql_table <- function(x, name = deparse(substitute(x)), ...) {
  path <- file.path(tempdir(), paste0(name, ".sqlite3"))
  
  test_con <- dbConnect(dbDriver("SQLite"), dbname = path)
  on.exit(dbDisconnect(test_con))
  
  object <- as.data.frame(x)
  dbWriteTable(test_con, name, x, row.names = FALSE, overwrite = TRUE)
  
  source_sqlite(path, name)
}

strip <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}

clone_sources <- function(df) {
  list(
    df = source_df(df),
    dt = source_dt(df),
    sqlite = make_sql_table(df)
  )
}

baseball_sources <- function() {
  data("baseball", package = "plyr")
  db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
  
  list(
    df = source_df(baseball),
    dt = source_dt(baseball),
    sqlite = source_sqlite(db_path, "baseball")
  )
}

players_sources <- function() {
  bball <- baseball_sources()
  list(
    df = group_by(bball$df, id),
    dt = group_by(bball$dt, id),
    sqlite = group_by(bball$sqlite, id)
  )
}
