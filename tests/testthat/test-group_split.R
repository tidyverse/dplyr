context("group_split")

test_that("group_split() keeps the grouping variables by default", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g)
  expect_equivalent(res, list(tbl[1:2,], tbl[3:4,]))
  expect_is(res, "vctrs_list_of")
  expect_equal(attr(res, "ptype"), tibble(x = integer(), g = factor(levels = c("a", "b"))))
})

test_that("group_split() can discard the grouping variables with keep = FALSE", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g, keep = FALSE)
  expect_equivalent(res, list(tbl[1:2, 1, drop = FALSE], tbl[3:4,1, drop = FALSE]))
  expect_is(res, "vctrs_list_of")
  expect_equal(attr(res, "ptype"), tibble(x = integer(), g = factor(levels = c("a", "b"))))
})

test_that("group_split() respects empty groups", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- group_split(tbl, g)
  expect_equivalent(res, list(tbl[1:2,], tbl[3:4,]))
  expect_is(res, "vctrs_list_of")
  expect_equal(attr(res, "ptype"), tibble(x = integer(), g = factor(levels = c("a", "b", "c"))))

  res <- group_split(tbl, g, .drop = FALSE)
  expect_equivalent(res, list(tbl[1:2,], tbl[3:4,], tbl[integer(), ]))
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
  expect_equal(length(chunks), 1L)
  expect_equal(bind_rows(chunks), setosa)

  chunks <- setosa %>% group_split(Species, .drop = FALSE)
  expect_equal(length(chunks), 3L)
  expect_equal(bind_rows(chunks), setosa)
})

test_that("group_split() works if no grouping column", {
  expect_equivalent(group_split(iris), list(iris))
})

test_that("group_split(keep=FALSE) does not try to remove virtual grouping columns (#4045)", {
  iris3 <- iris[1:3,]
  rows <- list(c(1L, 3L, 2L), c(3L, 2L, 3L))
  df <- new_grouped_df(
    iris3,
    groups = tibble(.bootstrap = 1:2, .rows := rows)
  )
  res <- group_split(df, keep = FALSE)

  expect_equivalent(
    res,
    list(iris3[rows[[1L]],], iris3[rows[[2L]],])
    )
})

test_that("group_split() respects .drop", {
  keys <- tibble(f = factor("b", levels = c("a", "b", "c"))) %>%
    group_keys(f, .drop = TRUE)
  expect_equal(nrow(keys), 1L)
})
