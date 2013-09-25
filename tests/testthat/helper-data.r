library(data.table)
library(RSQLite)

strip <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}

clone_tbls <- function(df) {
  list(
    df = tbl_df(df),
    dt = tbl_dt(df),
    sqlite = copy_to(src_tmp(), df, name = random_table_name())
  )
}

baseball_tbls <- function() {
  sql <- tbl(src_lahman(), "Batting")
  df <- collect(sql)
  dt <- tbl_dt(df)

  list(df = df, dt = dt, sqlite = sql)
}

players_tbls <- function() {
  bball <- baseball_tbls()
  list(
    df = group_by(bball$df, playerID),
    dt = group_by(bball$dt, playerID),
    sqlite = group_by(bball$sqlite, playerID)
  )
}
