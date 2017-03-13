context("lazyeval compatibility")

test_that("can select negatively (#2519)", {
  expect_identical(select_(mtcars, ~-cyl), mtcars[-2])
})
