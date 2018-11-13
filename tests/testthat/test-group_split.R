context("group_split")

test_that("group_split() works", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,]))
})

test_that("group_split() respects empty groups", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- group_split(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,], tbl[integer(), ]))
})

test_that("group_split.grouped_df() warns about ...", {
  expect_warning(group_split(group_by(mtcars, cyl), cyl))
})

test_that("group_split.grouped_df() works", {
  expect_equal(
    iris %>% group_by(Species) %>% group_split(),
    iris %>% group_split(Species)
  )
})

test_that("group_split_if works", {
  expect_equal(
    group_split_if(iris, is.factor),
    group_split(iris, Species)
  )
})

test_that("group_split_at works", {
  expect_equal(
    group_split_at(iris, vars("Species")),
    group_split(iris, Species)
  )
})

test_that("group_split / bind_rows round trip", {
  setosa <- iris %>%
    filter(Species == "setosa")

  chunks <- setosa %>% group_split(Species)
  expect_equal(length(chunks), 3L)

  expect_equal(bind_rows(chunks), setosa)
})
