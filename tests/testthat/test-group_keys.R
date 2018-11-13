context("group_keys()")

test_that("group_keys() works", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- group_keys(tbl, g)
  expect_equal(res, tibble(g = factor(c("a", "b", "c"))))
})

test_that("group_keys.grouped_df() warns about ...", {
  expect_warning(group_keys(group_by(mtcars, cyl), cyl))
})

test_that("group_keys.grouped_df() works", {
  expect_equal(
    iris %>% group_by(Species) %>% group_keys(),
    iris %>% group_keys(Species)
  )
})

test_that("group_keys_if works", {
  expect_equal(
    group_keys_if(iris, is.factor),
    group_keys(iris, Species)
  )
})

test_that("group_keys_at works", {
  expect_equal(
    group_keys_at(iris, vars("Species")),
    group_keys(iris, Species)
  )
})
