context("lazyeval compatibility")

test_that("can select negatively (#2519)", {
  expect_identical(select_(mtcars, ~-cyl), mtcars[-2])
})

test_that("select yields proper names", {
  expect_identical(names(select_(mtcars, ~cyl:hp)), c("cyl", "disp", "hp"))
})
