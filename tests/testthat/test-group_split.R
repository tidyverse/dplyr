context("group_split")

test_that("group_split() keeps the grouping variables by default", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g)
  expect_identical(res, list_of(tbl[1:2,], tbl[3:4,]))
  expect_is(res, "vctrs_list_of")
  expect_identical(attr(res, "ptype"), tibble(x = integer(), g = factor(levels = c("a", "b"))))
})

test_that("group_split() can discard the grouping variables with keep = FALSE", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2)))
  res <- group_split(tbl, g, keep = FALSE)
  expect_identical(res, list_of(tbl[1:2, 1, drop = FALSE], tbl[3:4,1, drop = FALSE]))
  expect_is(res, "vctrs_list_of")
  expect_identical(attr(res, "ptype"), tibble(x = integer()))
})

test_that("group_split() respects empty groups", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- group_split(tbl, g)
  expect_identical(res, list_of(tbl[1:2,], tbl[3:4,]))
  expect_is(res, "vctrs_list_of")
  expect_identical(attr(res, "ptype"), tibble(x = integer(), g = factor(levels = c("a", "b", "c"))))

  res <- group_split(tbl, g, .drop = FALSE)
  expect_identical(res, list_of(tbl[1:2,], tbl[3:4,], tbl[integer(), ]))
})

test_that("group_split.grouped_df() warns about ...", {
  expect_warning(group_split(group_by(mtcars, cyl), cyl))
})

test_that("group_split.rowwise_df() warns about ...", {
  expect_warning(group_split(rowwise(mtcars), cyl))
})

test_that("group_split.grouped_df() works", {
  iris <- as_tibble(iris)

  expect_identical(
    iris %>% group_by(Species) %>% group_split(),
    iris %>% group_split(Species)
  )
})

test_that("group_split / bind_rows round trip", {
  setosa <- iris %>% filter(Species == "setosa") %>% as_tibble()

  chunks <- setosa %>% group_split(Species)
  expect_identical(length(chunks), 1L)
  expect_identical(bind_rows(chunks), setosa)

  chunks <- setosa %>% group_split(Species, .drop = FALSE)
  expect_identical(length(chunks), 3L)
  expect_identical(bind_rows(chunks), setosa)
})

test_that("group_split() works if no grouping column", {
  expect_identical(group_split(iris), list_of(as_tibble(iris)))
})

test_that("group_split(keep=FALSE) does not try to remove virtual grouping columns (#4045)", {
  iris3 <- as_tibble(iris[1:3,])
  rows <- list(c(1L, 3L, 2L), c(3L, 2L, 3L))
  df <- new_grouped_df(
    iris3,
    groups = tibble(.bootstrap = 1:2, .rows := rows)
  )
  res <- group_split(df, keep = FALSE)

  expect_identical(
    res,
    list_of(iris3[rows[[1L]],], iris3[rows[[2L]],])
    )
})

test_that("group_split() respects .drop", {
  chunks <- tibble(f = factor("b", levels = c("a", "b", "c"))) %>%
    group_split(f, .drop = TRUE)
  expect_identical(length(chunks), 1L)
})

test_that("group_split() on a bare data frame returns bare tibbles", {
  df <- data.frame(x = 1:2)
  tib <- as_tibble(df)
  expect <- list_of(vec_slice(tib, 1), vec_slice(tib, 2))
  expect_identical(group_split(df, x), expect)
})

test_that("group_split() on a grouped df returns a list of tibbles", {
  df <- tibble(x = 1:2)
  gdf <- group_by(df, x)
  expect <- list_of(vec_slice(df, 1), vec_slice(df, 2))
  expect_identical(group_split(gdf), expect)
})

test_that("group_split() on a rowwise df returns a list of tibbles", {
  df <- tibble(x = 1:2)
  rdf <- rowwise(df)
  expect <- list_of(vec_slice(df, 1), vec_slice(df, 2))
  expect_identical(group_split(rdf), expect)
})

test_that("group_split() works with subclasses implementing group_by() / ungroup()", {
  local_foo_df()

  df <- list(x = c(1, 2, 2))
  df <- new_tibble(df, nrow = 3L, class = "foo_df")

  expect <- list_of(vec_slice(df, 1), vec_slice(df, 2:3))

  expect_identical(group_split(df, x), expect)
})

test_that("group_split() internally uses dplyr_row_slice()", {
  local_foo_df()

  df <- list(x = c(1, 2, 2))
  df <- new_tibble(df, nrow = 3L, class = "foo_df")

  local_methods(
    dplyr_row_slice.foo_df = function(x, i, ...) {
      abort(class = "dplyr_row_slice_called")
    }
  )

  expect_error(group_split(df, x), class = "dplyr_row_slice_called")
})
