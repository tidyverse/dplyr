context("data_frame")

test_that("data_frame returns correct number of rows with all combinatinos", {

  expect_equal(nrow(data_frame(value = 1:10)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me")), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10)), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10, value2 = 11:20)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)

})

test_that("can't make data_frame containing data.frame or array", {
  expect_error(data_frame(mtcars), "can not contain data.frames")
  expect_error(data_frame(diag(5)), "can not contain data.frames")
})


# as_data_frame -----------------------------------------------------------

test_that("columns must be same length", {
  l <- list(x = 1, y = 1:2)
  expect_error(as_data_frame(l), "not all same length")
})

test_that("columns must be named", {
  l1 <- list(1:10)
  l2 <- list(x = 1, 2)

  expect_error(as_data_frame(l1), "must be named")
  expect_error(as_data_frame(l2), "must be named")
})

test_that("can't coerce list data.frame or array", {
  expect_error(as_data_frame(list(x = mtcars)), "can not contain data.frames")
  expect_error(as_data_frame(list(x = diag(5))), "can not contain data.frames")
})
