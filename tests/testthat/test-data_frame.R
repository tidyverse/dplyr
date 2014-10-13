context("data_frame")
test_that("data_frame returns correct number of rows with all combinatinos", {
  expect_equal(nrow(data_frame(value = 1:10)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me")), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10)), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10, value2 = 11:20)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)
})
