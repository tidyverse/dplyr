context("tbl_cube")

expect_that("summarise works with single group", {
  by_month <- group_by(nasa, month)

  out <- summarise(by_month, temp = mean(temperature))

  expect_equal(names(out$dims), "month")
  expect_equal(names(out$mets), "temp")
  expect_equal(dim(out), c(12, 1))
})
