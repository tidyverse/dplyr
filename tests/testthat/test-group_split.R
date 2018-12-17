context("group_split")

test_that("group_split() keeps the grouping variables by default", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,]))
})

test_that("group_split() can discard the grouping variables with keep = FALSE", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g, keep = FALSE)
  expect_equal(res, list(tbl[1:2, 1, drop = FALSE], tbl[3:4,1, drop = FALSE]))
})

test_that("group_split() respects empty groups", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- group_split(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,], tbl[integer(), ]))
})

test_that("group_split.grouped_df() warns about ...", {
  expect_warning(group_split(group_by(mtcars, cyl), cyl))
})

test_that("group_split.rowwise_df() warns about ...", {
  expect_warning(group_split(rowwise(mtcars), cyl))
})

test_that("group_split.grouped_df() works", {
  expect_equal(
    iris %>% group_by(Species) %>% group_split(),
    iris %>% group_split(Species)
  )
})

test_that("group_split / bind_rows round trip", {
  setosa <- iris %>%
    filter(Species == "setosa")

  chunks <- setosa %>% group_split(Species)
  expect_equal(length(chunks), 3L)

  expect_equal(bind_rows(chunks), setosa)
})

test_that("group_split() works if no grouping column", {
  expect_equal(group_split(iris), list(iris))
})
