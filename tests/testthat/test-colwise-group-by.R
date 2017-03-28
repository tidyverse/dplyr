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
