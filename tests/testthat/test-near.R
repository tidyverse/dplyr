context("near")

test_that("near accepts nearby fp values", {
  expect_true(near(sqrt(2)^2, 2))
})
