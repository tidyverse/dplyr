context("split_by")

test_that("split_by() works", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- split_by(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,]))
})

test_that("split_by() respects empty groups", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- split_by(tbl, g)
  expect_equal(res, list(tbl[1:2,], tbl[3:4,], tbl[integer(), ]))
})

test_that("split.grouped_df() methods only works without arguments", {
  expect_error(split(group_by(mtcars, cyl), cyl), "split() on a grouped tibble is only supported without arguments, consider split_by()", fixed = TRUE)
  expect_error(split(rowwise(mtcars), cyl), "split() on a rowwise tibble is only supported without arguments, consider split_by()", fixed = TRUE)
})

test_that("split.grouped_df() works", {
  expect_equal(
    iris %>% group_by(Species) %>% split(),
    iris %>% split_by(Species)
  )
})

test_that("split.rowwise_df() works", {
  res <- iris %>% rowwise() %>% split()
  expect_equal(res, lapply(seq_len(nrow(iris)), function(.) as_tibble(iris[., ])))
})

test_that("split.tbl_df() aborts", {
  expect_error(split(tibble()), "split() not supported for ungrouped tibbles, you probably need split_by()", fixed = TRUE)
})

test_that("split_by_if works", {
  expect_equal(
    split_by_if(iris, is.factor),
    split_by(iris, Species)
  )
})

test_that("split_by_at works", {
  expect_equal(
    split_by_at(iris, vars("Species")),
    split_by(iris, Species)
  )
})

test_that("split_by/bind_rows round trip", {
  setosa <- iris %>%
    filter(Species == "setosa")

  chunks <- setosa %>% split_by(Species)
  expect_equal(length(chunks), 3L)

  expect_equal(bind_rows(chunks), setosa)
})
