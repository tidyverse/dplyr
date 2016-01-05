context("matrix")

test_that("correct rows and cols", {
  x <- matrix(1:6, nrow = 2)
  out <- as_data_frame(x)

  expect_equal(dim(out), c(2, 3))
})

test_that("preserves col names", {
  x <- matrix(1:4, nrow = 2)
  colnames(x) <- c("a", "b")

  out <- as_data_frame(x)
  expect_equal(names(out), c("a", "b"))
})

test_that("preserves attributes except dim and names", {
  date <- Sys.Date() + 0:1
  dim(date) <- c(2, 1)
  colnames(date) <- "a"

  out <- as_data_frame.matrix(date)
  expect_equal(attributes(out[[1]])$name, NULL)
  expect_equal(attributes(out[[1]])$class, "Date")
})

test_that("handles atomic vectors", {
  x <- matrix(TRUE, nrow = 2)
  out <- as_data_frame(x)
  expect_equal(out[[1]], c(TRUE, TRUE))

  x <- matrix(1L, nrow = 2)
  out <- as_data_frame(x)
  expect_equal(out[[1]], c(1L, 1L))

  x <- matrix(1.5, nrow = 2)
  out <- as_data_frame(x)
  expect_equal(out[[1]], c(1.5, 1.5))

  x <- matrix("a", nrow = 2)
  out <- as_data_frame(x)
  expect_equal(out[[1]], c("a", "a"))

  x <- matrix(complex(real = 1, imag = 2), nrow = 2)
  out <- as_data_frame(x)
  expect_equal(out[[1]], as.vector(x))
})

test_that("auto-assigning names", {
  expect_identical(as_data_frame(diag(3L)),
                   as_data_frame(as.data.frame(diag(3L))))
})
