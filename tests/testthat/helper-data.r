library(data.table)
library(RSQLite)

make_sql_table <- function(x, name = deparse(substitute(x)), ...) {
  path <- file.path(tempdir(), paste0(name, ".sqlite3"))

  test_con <- dbConnect(dbDriver("SQLite"), dbname = path)
  on.exit(dbDisconnect(test_con))

  object <- as.data.frame(x)
  dbWriteTable(test_con, name, x, row.names = FALSE, overwrite = TRUE)

  tbl_sqlite(path, name)
}

strip <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}

clone_tbls <- function(df) {
  list(
    df = tbl_df(df),
    dt = tbl_dt(df),
    sqlite = make_sql_table(df)
  )
}

baseball_tbls <- function() {
  data("baseball", package = "plyr")
  db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")

  list(
    df = tbl_df(baseball),
    dt = tbl_dt(baseball),
    sqlite = tbl_sqlite(db_path, "baseball")
  )
}

players_tbls <- function() {
  bball <- baseball_tbls()
  list(
    df = group_by(bball$df, id),
    dt = group_by(bball$dt, id),
    sqlite = group_by(bball$sqlite, id)
  )
}
