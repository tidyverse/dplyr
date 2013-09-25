context("Equivalence (grouped)")

players <- players_tbls()

test_that("group size the same regardless of data tbl", {
  gs_df <- group_size(players$df)
  gs_dt <- group_size(players$df)
  gs_db <- group_size(players$sqlite)

  expect_equal(gs_dt, gs_df)
  expect_equal(gs_db, gs_df)
})

test_that("n the same regardless of tbl", {
  count_df <- summarise(players$df, count = n())
  count_dt <- ungroup(summarise(players$dt, count = n()))
  count_db <- summarise(players$sqlite, count = n())

  expect_equal(count_dt$n, count_df$n)
  expect_equal(count_db$n, count_df$n)
})

test_that("filter the same regardless of tbl", {
  filter_df <- strip(filter(players$df, AB == max(AB)))
  filter_dt <- strip(filter(players$dt, AB == max(AB)))
  # sqlite doesn't supported windowed filters
  
  expect_equivalent(filter_dt, filter_df)
})

test_that("arrange the same regardless of tbl", {
  arrange_df <- strip(arrange(players$df, AB, desc(G)))
  arrange_dt <- strip(arrange(players$dt, AB, desc(G)))
  # sqlite doesn't supported windowed arranges
  
  expect_equivalent(arrange_dt, arrange_df)
})

test_that("mutate the same regardless of tbl", {
  mutate_df <- strip(mutate(players$df, cyear = yearID - min(yearID) + 1))
  mutate_dt <- strip(mutate(players$dt, cyear = yearID - min(yearID) + 1))
  # sqlite doesn't supported windowed mutates
  
  expect_equivalent(mutate_dt, mutate_df)
})
