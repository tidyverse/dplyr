context("colwise group_by")

test_that("group_by_ verbs take scoped inputs", {
  expect_identical(group_vars(group_by_all(mtcars)), names(mtcars))
  expect_identical(group_vars(group_by_at(mtcars, vars(starts_with("d")))), c("disp", "drat"))
  expect_identical(group_vars(group_by_if(iris, is.factor)), "Species")
})

test_that("group_by_ verbs accept optional operations", {
  df <- data_frame(x = 1:2, y = 2:3)
  gdf <- group_by(mutate_all(df, as.factor), x, y)

  expect_identical(group_by_all(df, as.factor), gdf)
  expect_identical(group_by_if(df, is_integer, as.factor), gdf)
  expect_identical(group_by_at(df, vars(x:y), as.factor), gdf)
})

test_that("group_by_at can group by an already grouped by data (#3351)", {
  tbl <- data_frame(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  res <- group_by_at(tbl, vars(one_of("gr1", "gr2")))
  expect_equal( groups(res), list( sym("gr1"), sym("gr2")) )
})
