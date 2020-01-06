context("if_else")

test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(if_else(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)

  expect_equal(if_else(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(if_else(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(if_else(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})

test_that("works with lists", {
  x <- list(1, 2, 3)

  expect_equal(
    if_else(c(TRUE, TRUE, FALSE), x, list(NULL)),
    list(1, 2, NULL)
  )
})

# Errors ------------------------------------------------------------------

test_that("if_else() give meaningful errors", {
  verify_output(test_path("test-if-else-errors.txt"), {

    if_else(1:10, 1, 2)
    if_else(1:3 < 2, 1:2, 1:3)
    if_else(1:3 < 2, 1:3, 1:2)
    if_else(1:3 < 2, 1, 1L)

    x <- factor("x")
    y <- ordered("x")
    if_else(1:3 < 2, x, y)
  })
})
