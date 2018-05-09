context("test-rows.R")

test_that("rows works", {
  df <- data.frame(x=c(1,1,2,2))
  expect_equal(rows(df), list(0:3))
  expect_equal(rows(group_by(df,x)), list(0:1, 2:3))
  expect_equal(rows(rowwise(df)), as.list(0:3))
})

test_that("group_data works", {
  df <- tibble(x = c(1,1,2,2))

  expect_identical(
    group_data(df),
    tibble(.rows=list(0:3))
  )

  expect_identical(
    group_by(df,x) %>% group_data(),
    tibble(x = c(1,2), .rows = list(0:1, 2:3))
  )

  expect_identical(
    rowwise(df) %>% group_data(),
    tibble(.rows = as.list(0:3))
  )
})
