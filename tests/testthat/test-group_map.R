context("group_map")

test_that("group_map() makes a grouped_df", {
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

test_that("group_map() wants functions with at least 2 arguments, or ... (#3996)", {
  head1 <- function(d) head(d, 1)

  g <- iris %>%
    group_by(Species)

  expect_error(group_map(g, head1), "The function must accept at least two arguments")

  head1 <- function(d, ...) head(d, 1)
  expect_equal(length(group_map(g, head1)), 3L)
})

test_that("group_map() works on ungrouped data frames (#4067)", {
  expect_identical(
    group_map(mtcars, ~ head(.x, 2L)),
    list(head(mtcars, 2L))
  )
})
