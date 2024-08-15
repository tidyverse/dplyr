test_that("Empty matrix can be coerced to a data frame (#7004)", {
  skip_if_not(getRversion() >= "4.4")
  expect_equal(
    slice(as.data.frame(matrix(nrow = 0, ncol = 0)), 1),
    data.frame()
  )
})
