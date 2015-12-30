context("tbl_df")

test_that("[ never drops", {
  mtcars2 <- tbl_df(mtcars)
  expect_is(mtcars2[, 1], "data.frame")
  expect_is(mtcars2[, 1], "tbl_df")
  expect_equal(mtcars2[, 1], mtcars2[1])
})

test_that("[ with 0 cols creates correct row names (#656)", {
  zero_row <- tbl_df(iris)[, 0]
  expect_is(zero_row, "tbl_df")
  expect_equal(nrow(zero_row), 150)
  expect_equal(ncol(zero_row), 0)
  expect_output(print(zero_row), "[150 x 0]", fixed = TRUE)
})

test_that("[ with 0 cols creates correct row names (#656)", {
  zero_row <- tbl_df(iris)[0]
  expect_is(zero_row, "tbl_df")
  expect_equal(nrow(zero_row), 150)
  expect_equal(ncol(zero_row), 0)
  expect_output(print(zero_row), "[150 x 0]", fixed = TRUE)
})

test_that("[.tbl_df is careful about names (#1245)",{
  foo <- data_frame(x = 1:10, y = 1:10)
  expect_error( foo["z"] )
  expect_error( foo[ c("x", "y", "z") ] )

  expect_error( foo[, "z"] )
  expect_error( foo[, c("x", "y", "z") ] )
})

test_that("[[.tbl_df ignores exact argument",{
  foo <- data_frame(x = 1:10, y = 1:10)
  expect_warning(foo[["x"]], NA)
  expect_warning(foo[["x", exact = FALSE]], "ignored")
  expect_identical(getElement(foo, "y"), 1:10)
})
