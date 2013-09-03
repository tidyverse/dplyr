context("Equivalence (grouped)")

players <- players_tbls()

test_that("group size the same regardless of data tbl", {
  gs_df <- group_size(players$df)
  gs_dt <- group_size(players$df)
  gs_db <- group_size(players$sqlite)

  expect_equal(gs_dt, gs_df)
  expect_equal(gs_db, gs_df)
})

test_that("n that same regardless of tbl", {
  count_df <- summarise(players$df, count = n())
  count_dt <- ungroup(summarise(players$dt, count = n()))
  count_db <- summarise(players$sqlite, count = n())

  expect_equal(count_dt$n, count_df$n)
  expect_equal(count_db$n, count_df$n)
})
