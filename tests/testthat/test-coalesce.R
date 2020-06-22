context("coalesce")

test_that("non-missing scalar replaces all missing values", {
  x <- c(NA, 1)
  expect_equal(coalesce(x, 1), c(1, 1))
})

test_that("coerces to common type", {
  expect_identical(coalesce(NA, 1), 1)

  f <- factor("x", levels = c("x", "y"))
  expect_identical(coalesce(NA, f), f)
})

test_that("finds non-missing values in multiple positions", {
  x1 <- c(1L, NA, NA)
  x2 <- c(NA, 2L, NA)
  x3 <- c(NA, NA, 3L)

  expect_equal(coalesce(x1, x2, x3), 1:3)
})

test_that("coalesce() gives meaningful error messages", {
  verify_output(test_path("test-coalesce-errors.txt"), {
    coalesce(1:2, 1:3)
    coalesce(1:2, letters[1:2])
  })
})

test_that("coalesce() supports data frames (#5326)", {
  out <- coalesce(
    data.frame(x = c(NA, 1)),
    data.frame(x = 1:2)
  )
  expect_identical(out, data.frame(x = c(1, 1)))

  df1 <- data.frame(x = c(NA, 1, NA), y = c(2, NA, NA), z = c(1:2, NA))
  df2 <- tibble::tibble(x = 1:3, y = c(3, 4, NA), z = c(NA, NA, NA))
  df3 <- data.frame(x = NA, y = c(30, 40, 50), z = 101:103)
  out <- coalesce(df1, df2, df3)
  exp <- tibble(x = c(1, 1, 3), y = c(2, 4, 50), z = c(1L, 2L, 103L))
  expect_identical(out, exp)

  expect_error(
    coalesce(
      data.frame(x = c(NA, 1)),
      data.frame(x = c("a", "b"))
    ),
    class = "vctrs_error_incompatible_type"
  )

  expect_error(coalesce(as.matrix(mtcars), as.matrix(mtcars)), "matrices")
})
