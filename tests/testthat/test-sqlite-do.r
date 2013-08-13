context("SQLite: do")

bball <- baseball_sources()

test_that("Results respect select", {
  by_team_2 <- group_by(select(bball$sqlite, year, team), team)
  by_team_all <- group_by(bball$sqlite, team)

  ncols <- function(group) unique(unlist(do(group, ncol)))

  expect_equal(ncols(by_team_2), 3)
  expect_equal(ncols(by_team_all), ncol(baseball))
})

test_that("Results independent of chunk_size", {
  nrows <- function(group, n) unlist(do(group, nrow, .chunk_size = n))

  by_team <- group_by(select(bball$sqlite, year, team), team)
  team_sizes <- as.vector(table(bball$df$team))

  expect_equal(nrows(by_team, 1e5), team_sizes) # 1 chunk
  expect_equal(nrows(by_team, 1e4), team_sizes) # 3 chunks
  expect_equal(nrows(by_team, 1e3), team_sizes) # 21 chunks
  expect_equal(nrows(by_team, 999), team_sizes) # 22 chunks
})
