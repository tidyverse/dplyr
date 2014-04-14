context("Lead and lag")

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_is(lead(x), "Date")
  expect_is(lag(x),  "Date")

  expect_is(lead(y), "POSIXct")
  expect_is(lag(y),  "POSIXct")
})

