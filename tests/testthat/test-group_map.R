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

