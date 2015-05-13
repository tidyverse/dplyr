context("Window functions")

test_that("If n = 0, lead and lag return x", {
  expect_equal(lead(1:2, 0), 1:2)
  expect_equal(lag(1:2, 0), 1:2)
})

test_that("If n = length(x), returns all missing", {
  miss <- rep(NA_integer_, 2)
  
  expect_equal(lead(1:2, 2), miss)
  expect_equal(lag(1:2, 2), miss)
  
})

test_that("cumany handles NA (#408)", {
  batman <- c(NA,NA,NA,NA,NA)
  expect_true(all(is.na(cumany(batman))))
  expect_true(all(is.na(cumall(batman))))
  
  x <- c(FALSE,NA)
  expect_true( all( !cumall(x) ) )
  
  x <- c(TRUE,NA)
  expect_true( all( cumany(x) ) )
  
})

test_that("percent_rank ignores NAs (#1132)", {
  testdata <- c(1:4, rep(NA, 4))
  expect_equal( percent_rank(testdata), (min_rank(testdata) - 1) / (sum(!is.na(testdata)) - 1) )
  expect_equal( percent_rank(testdata), (min_rank(testdata) - 1) / (length(testdata[!is.na(testdata)]) - 1) )
})

test_that("cume_dist ignores NAs (#1132)", {
  testdata <- c(1:4, rep(NA, 4))
  expect_equal( cume_dist(testdata), rank(testdata, ties.method = "max", na.last = "keep") / sum(!is.na(testdata) ) )
  expect_equal( cume_dist(testdata), rank(testdata, ties.method = "max", na.last = "keep") / length(testdata[!is.na(testdata)] ) )
})


