context("Filter - windowed")

test_that("filter calls windowed versions of sql functions", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      filter(row_number(x) < 3) %>%
      collect()
    expect_equal(res$x, c(1, 2, 6, 7))
    expect_equal(res$g, c(1, 1, 2, 2))
  }

  df <- data_frame(x = 1:10, g = rep(c(1, 2), each = 5))
  tbls <- test_load(df, ignore = "sqlite") # SQLite doesn't support window functions
  tbls %>% lapply(test_f)
})

test_that("recycled aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      filter(x > mean(x)) %>%
      collect()
    expect_equal(res$x, c(4, 5, 9, 10))
    expect_equal(res$g, c(1, 1, 2, 2))
  }

  df <- data_frame(x = 1:10, g = rep(c(1, 2), each = 5))
  tbls <- test_load(df, ignore = "sqlite") # SQLite doesn't support window functions
  tbls %>% lapply(test_f)
})

test_that("cumulative aggregates generate window function", {
  test_f <- function(tbl) {
    res <- tbl %>%
      group_by(g) %>%
      arrange(x) %>%
      filter(cumsum(x) > 3) %>%
      collect()
    expect_equal(res$x, c(3, 3, 4))
    expect_equal(res$g, c(1, 2, 2))
  }

  df <- data_frame(x = c(1:3, 2:4), g = rep(c(1, 2), each = 3))
  tbls <- test_load(df, ignore = "sqlite") # SQLite doesn't support window functions
  tbls %>% lapply(test_f)
})
