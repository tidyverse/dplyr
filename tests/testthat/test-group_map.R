context("group_map")

test_that("group_map() respects empty groups", {
  res <- group_by(mtcars, cyl) %>%
    group_map(~ head(.x, 2L))
  expect_equal(length(res), 3L)

  res <- iris %>%
    group_by(Species) %>%
    filter(Species == "setosa") %>%
    group_map(~ tally(.x))
  expect_equal(length(res), 1L)

  res <- iris %>%
    group_by(Species, .drop = FALSE) %>%
    filter(Species == "setosa") %>%
    group_map(~ tally(.x))
  expect_equal(length(res), 3L)
})

test_that("group_map() can return arbitrary objects", {
  expect_equal(
    group_by(mtcars, cyl) %>% group_map(~ 10),
    rep(list(10), 3)
  )
})

test_that("group_map() works on ungrouped data frames (#4067)", {
  expect_identical(
    group_map(mtcars, ~ head(.x, 2L)),
    list(head(mtcars, 2L))
  )
})

test_that("group_modify() makes a grouped_df", {
  res <- group_by(mtcars, cyl) %>%
    group_modify(~ head(.x, 2L))

  expect_equal(nrow(res), 6L)
  expect_equal(group_rows(res), list_of(1:2, 3:4, 5:6))

  res <- iris %>%
    group_by(Species) %>%
    filter(Species == "setosa") %>%
    group_modify(~ tally(.x))
  expect_equal(nrow(res), 1L)
  expect_equal(group_rows(res), list_of(1L))

  res <- iris %>%
    group_by(Species, .drop = FALSE) %>%
    filter(Species == "setosa") %>%
    group_modify(~ tally(.x))
  expect_equal(nrow(res), 3L)
  expect_equal(group_rows(res), list_of(1L, 2L, 3L))
})

test_that("group_modify() and group_map() want functions with at least 2 arguments, or ... (#3996)", {
  head1 <- function(d, ...) head(d, 1)

  g <- iris %>% group_by(Species)
  expect_equal(nrow(group_modify(g, head1)), 3L)
  expect_equal(length(group_map(g, head1)), 3L)
})

test_that("group_modify() works on ungrouped data frames (#4067)", {
  expect_identical(
    group_modify(mtcars, ~ head(.x, 2L)),
    head(mtcars, 2L)
  )
})

test_that("group_map() uses ptype on empty splits (#4421)", {
  res <- mtcars %>%
    group_by(cyl) %>%
    filter(hp > 1000) %>%
    group_map(~.x)
  expect_equivalent(res, list())
  ptype <- attr(res, "ptype")
  expect_equal(names(ptype), setdiff(names(mtcars), "cyl"))
  expect_equal(nrow(ptype), 0L)
  expect_is(ptype, "data.frame")
})

test_that("group_modify() uses ptype on empty splits (#4421)", {
  res <- mtcars %>%
    group_by(cyl) %>%
    filter(hp > 1000) %>%
    group_modify(~.x)
  expect_equal(res, group_by(mtcars[integer(0L), names(res)], cyl))
})

test_that("group_modify() works with additional arguments (#4509)", {
  myfun <- function(.x, .y, foo) {
    .x[[foo]] <- 1
    .x
  }

  srcdata <-
    data.frame(
      A=rep(1:2, each = 3)
    ) %>%
    group_by(A)
  targetdata <- srcdata
  targetdata$bar <- 1

  expect_equal(
    group_modify(.data = srcdata, .f = myfun, foo = "bar"),
    targetdata
  )
})
test_that("group_map() give meaningful errors", {
  head1 <- function(d) head(d, 1)

  verify_output(test_path("test-group_map-errors.txt"), {
    "# group_modify()"
    mtcars %>% group_by(cyl) %>% group_modify(~ data.frame(cyl = 19))
    mtcars %>% group_by(cyl) %>% group_modify(~ 10)
    iris %>% group_by(Species) %>% group_modify(head1)

    "# group_map()"
    iris %>% group_by(Species) %>% group_map(head1)
  })
})
