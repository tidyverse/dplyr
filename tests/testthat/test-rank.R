context("rank")

ntile_h <- function(x, n) {
  tibble(x = x) %>%
    mutate(y = ntile(x, n)) %>%
    pull(y)
}

test_that("ntile ignores number of NAs", {
  x <- c(1:3, NA, NA, NA)

  expect_equal(ntile(x, 3), x)
  expect_equal(ntile_h(x, 3), x)

  x1 <- c(1L, 1L, 1L, NA, NA, NA)
  expect_equal(ntile(x, 1), x1)
  expect_equal(ntile_h(x, 1), x1)
})

test_that("ntile always returns an integer", {
  expect_equal(ntile(numeric(), 3), integer())
  expect_equal(ntile_h(numeric(), 3), integer())

  expect_equal(ntile(NA, 3), NA_integer_)
  expect_equal(ntile_h(NA, 3), NA_integer_)
})
