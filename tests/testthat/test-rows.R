context("test-rows.R")

test_that("rows works", {
  df <- data.frame(x=c(1,1,2,2))
  expect_equal(rows(df), list(0:3))
  expect_equal(rows(group_by(df,x)), list(0:1, 2:3))
  expect_equal(rows(rowwise(df)), as.list(0:3))
})
