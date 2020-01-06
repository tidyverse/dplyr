context("rank")

ntile_h <- function(x, n) {
  tibble(x = x) %>%
    mutate(y = ntile(x, n)) %>%
    pull(y)
}

ntile_h_dplyr <- function(x, n) {
  tibble(x = x) %>%
    mutate(y = dplyr::ntile(x, n)) %>%
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

test_that("ntile handles character vectors consistently", {
  charvec_sort_test <- function() {
    x1 <- c("[", "]", NA, "B", "y", "a", "Z")
    x2 <- c("a", "b", "C")

    expect_equal(ntile_h(x1, 3), ntile_h_dplyr(x1, 3))
    expect_equal(ntile_h(x2, 2), ntile_h_dplyr(x2, 2))
  }

  # Test against both the local, and the C locale for collation
  charvec_sort_test()
  withr::with_collate("C", charvec_sort_test())
})

test_that("ntile() does not overflow (#4186)", {
  res <- tibble(a = 1:1e5) %>%
    mutate(b = ntile(n = 1e5)) %>%
    count(b) %>%
    pull()

  expect_true(all(res == 1L))
})

