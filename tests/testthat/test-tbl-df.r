context("tbl_df/grouped_df")

test_that("grouped_df returns tbl_df if no groups", {
  df <- grouped_df(mtcars, list())
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
})

test_that("[ never drops", {
  mtcars2 <- tbl_df(mtcars)
  expect_is(mtcars2[, 1], "data.frame")
  expect_is(mtcars2[, 1], "tbl_df")
  expect_equal(mtcars2[, 1], mtcars2[1])
})

test_that("[ with 0 cols creates correct row names (#656)", {
  zero_row <- iris %>% tbl_df() %>% `[`(, 0)
  expect_is(zero_row, "tbl_df")
  expect_equal(nrow(zero_row), 150)
  expect_equal(ncol(zero_row), 0)
  expect_output(print(zero_row), "[150 x 0]", fixed = TRUE)
})

test_that("[ with 0 cols creates correct row names (#656)", {
  zero_row <- iris %>% tbl_df() %>% `[`(0)
  expect_is(zero_row, "tbl_df")
  expect_equal(nrow(zero_row), 150)
  expect_equal(ncol(zero_row), 0)
  expect_output(print(zero_row), "[150 x 0]", fixed = TRUE)
})
