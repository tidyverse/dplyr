context("percent_rank and cume_dist")


test_that("#1132 is fixed - percent_rank", {
  testdata <- c(1:4, rep(NA, 4))
  res <- percent_rank(testdata)
  manually_calculated_res <- (min_rank(testdata) - 1) / (sum(!is.na(testdata)) - 1)
  manually_calculated_res2 <- (min_rank(testdata) - 1) / (length(testdata[!is.na(testdata)]) - 1)
  expect_equal( res, manually_calculated_res)
  expect_equal( res, manually_calculated_res2)
})

test_that("#1132 is fixed - cume_dist", {
  testdata <- c(1:4, rep(NA, 4))
  res <- cume_dist(testdata)
  manually_calculated_res <- rank(testdata, ties.method = "max", na.last = "keep") / sum(!is.na(testdata))
  manually_calculated_res2 <- rank(testdata, ties.method = "max", na.last = "keep") / length(testdata[!is.na(testdata)])
  expect_equal( res, manually_calculated_res)
  expect_equal( res, manually_calculated_res2)
})
