context("lst")

test_that("lst handles named and unnamed NULL arguments", {
  expect_equivalent(lst(NULL), list("NULL" = NULL))
  expect_identical(lst(a = NULL), list(a = NULL))
  expect_identical(lst(NULL, b = NULL, 1:3),
                   list("NULL" = NULL, b = NULL, "1:3" = 1:3))
})

test_that("lst handles internal references", {
  expect_identical(lst(a = 1, b = a), list(a = 1, b = 1))
  expect_identical(lst(a = NULL, b = a), list(a = NULL, b = NULL))
})
