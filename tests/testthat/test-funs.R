
test_that("returns NA if any argument is NA", {
  expect_equal(between(1, 1, NA), NA)
  expect_equal(between(1, NA, 1), NA)
  expect_equal(between(NA, 1, 1), NA)
})

test_that("compatible with base R", {
  x <- runif(1e3)
  expect_equal(between(x, 0.25, 0.5), x >= 0.25 & x <= 0.5)
})

test_that("warns when called on S3 object", {
  expect_warning(between(structure(c(1, 5), class = "foo"), 1, 3), "numeric vector with S3 class")
  expect_warning(between(factor("x"), 1, 2), "S3 class")
})

test_that("unless it's a date or date time", {
  expect_warning(between(Sys.Date(), 1, 3), NA)
  expect_warning(between(Sys.time(), 1, 3), NA)
})
