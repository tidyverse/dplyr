context("data_frame")
test_that("data_frame returns correct number of rows with all combinatinos", {

  expect_equal(nrow(data_frame(value = 1:10)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me")), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10)), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10, value2 = 11:20)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)

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
