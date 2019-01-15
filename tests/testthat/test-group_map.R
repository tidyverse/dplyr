context("group_map")

test_that("group_map() makes a grouped_df", {
  res <- group_by(mtcars, cyl) %>%
    group_map(~ head(.x, 2L))

  expect_equal(nrow(res), 6L)
  expect_equal(group_rows(res), list(1:2, 3:4, 5:6))

  res <- iris %>%
    group_by(Species) %>%
    filter(Species == "setosa") %>%
    group_map(~ tally(.x))
  expect_equal(nrow(res), 1L)
  expect_equal(group_rows(res), list(1L))

  res <- iris %>%
    group_by(Species, .drop = FALSE) %>%
    filter(Species == "setosa") %>%
    group_map(~ tally(.x))
  expect_equal(nrow(res), 3L)
  expect_equal(group_rows(res), list(1L, 2L, 3L))
})

test_that("group_map() rejects non data frames", {
  expect_error(
    group_by(mtcars, cyl) %>% group_map(~ 10)
  )
})

test_that("group_map() rejects data frames that contain the grouping variable", {
  expect_error(
    group_by(mtcars, cyl) %>% group_map(~ data.frame(cyl = 19))
  )
})

test_that("group_map() wants functions with at least 2 arguments, or ... (#3996)", {
  head1 <- function(d) head(d, 1)

  g <- iris %>%
    group_by(Species)

  expect_error(group_map(g, head1), "The function must accept at least two arguments")

  head1 <- function(d, ...) head(d, 1)
  expect_equal(nrow(group_map(g, head1)), 3L)
})

test_that("group_map() works on ungrouped data frames (#4067)", {
  expect_identical(
    group_map(mtcars, ~ head(.x, 2L)),
    head(mtcars, 2L)
  )
})
