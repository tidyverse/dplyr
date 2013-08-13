context("Equivalence (grouped)")

library(data.table)
data("baseball", package = "plyr")
player_df <- group_by(baseball, id)
player_dt <- group_by(data.table(baseball), id)

db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_s <- source_sqlite(db_path, "baseball")

player_db <- group_by(baseball_s, id)

test_that("group size the same regardless of data source", {
  gs_df <- group_size(player_df)
  gs_dt <- group_size(player_dt)
  gs_db <- group_size(player_db)
  
  expect_equal(gs_dt, gs_df)
  expect_equal(gs_db, gs_df)
})

test_that("n that same regardless of source", {
  count_df <- summarise(player_df, count = n())
  count_dt <- ungroup(summarise(player_dt, count = n()))
  count_db <- summarise(player_db, count = n())
  
  expect_equal(count_dt$n, count_df$n)
  expect_equal(count_db$n, count_df$n)
})
