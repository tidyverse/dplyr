context("colwise group_by")

test_that("group_by_ verbs take scoped inputs", {
  expect_identical(group_vars(group_by_all(mtcars)), names(mtcars))
  expect_identical(group_vars(group_by_at(mtcars, vars(starts_with("d")))), c("disp", "drat"))
  expect_identical(group_vars(group_by_if(iris, is.factor)), "Species")
})
