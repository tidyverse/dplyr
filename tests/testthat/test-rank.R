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


test_that("row_number handles empty data frames (#762)", {
  df <- data.frame(a = numeric(0))
  res <- df %>% mutate(
    row_number_0 = row_number(),
    row_number_a = row_number(a),
    ntile = ntile(a, 2),
    min_rank = min_rank(a),
    percent_rank = percent_rank(a),
    dense_rank = dense_rank(a),
    cume_dist = cume_dist(a)
  )
  expect_equal(
    names(res),
    c("a", "row_number_0", "row_number_a", "ntile", "min_rank", "percent_rank", "dense_rank", "cume_dist")
  )
  expect_equal(nrow(res), 0L)
})


test_that("lead/lag inside mutate handles expressions as value for default (#1411) ", {
  df <- tibble(x = 1:3)
  res <- mutate(df, leadn = lead(x, default = x[1]), lagn = lag(x, default = x[1]))
  expect_equal(res$leadn, lead(df$x, default = df$x[1]))
  expect_equal(res$lagn, lag(df$x, default = df$x[1]))

  res <- mutate(df, leadn = lead(x, default = c(1)), lagn = lag(x, default = c(1)))
  expect_equal(res$leadn, lead(df$x, default = 1))
  expect_equal(res$lagn, lag(df$x, default = 1))
})
