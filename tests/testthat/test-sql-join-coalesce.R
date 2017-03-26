context("SQL: join with coalesce")

test_that("right join with coalesce = FALSE on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = "x", coalesce = FALSE) %>%
      arrange(x, y, z) %>%
      collect()
    cat(class(tbl_left), "\n")
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5)
  df_right <- data_frame(x = 1:6, z = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join with coalesce = FALSE on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, by = "x", coalesce = FALSE) %>%
      arrange(x, y, z) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5)
  df_right <- data_frame(x = 1:6, z = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right join with coalesce = TRUE on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = "x", coalesce = TRUE) %>%
      arrange(x, y, z) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5)
  df_right <- data_frame(x = 1:6, z = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join with coalesce = TRUE on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <- full_join(tbl_left, tbl_right, by = "x", coalesce = TRUE) %>%
      arrange(x, y, z) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5)
  df_right <- data_frame(x = 1:6, z = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right join with coalesce = FALSE on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = c("xl" = "xr"), coalesce = FALSE) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equal(as.integer(res$xl), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(xl = 1:5, y = 1:5)
  df_right <- data_frame(xr = 1:6, z = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join with coalesce = FALSE on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, by = c("xl" = "xr"), coalesce = FALSE) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equal(as.integer(res$xl), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(xl = 1:5, y = 1:5)
  df_right <- data_frame(xr = 1:6, z = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right join with coalesce = TRUE on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = c("xl" = "xr"), coalesce = TRUE) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equal(as.integer(res$xl), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(xl = 1:5, y = 1:5)
  df_right <- data_frame(xr = 1:6, z = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join with coalesce = TRUE on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, by = c("xl" = "xr"), coalesce = TRUE) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equal(as.integer(res$xl), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(xl = 1:5, y = 1:5)
  df_right <- data_frame(xr = 1:6, z = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right natural join with coalesce = FALSE", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, coalesce = FALSE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, NA))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = 1:6, y = 1:6, w = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full natural join with coalesce = FALSE", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, coalesce = FALSE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, NA))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = 1:6, y = 1:6, w = 1:6)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right natural join with coalesce = TRUE", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, coalesce = TRUE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, 6L))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = 1:6, y = 1:6, w = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full natural join with coalesce = TRUE", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, coalesce = TRUE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, 6L))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, 6L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = 1:6, y = 1:6, w = 1:6)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right natural join with coalesce = FALSE and partial match", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, coalesce = FALSE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, NA))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = c(1:4, 7), y = 1:5, w = 1:5)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full natural join with coalesce = FALSE and partial match", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, coalesce = FALSE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, NA))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, NA))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = c(1:4, 7), y = 1:5, w = 1:5)
  # this test is only for SQL sources (data frame sources always coalesce)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("df", "sqlite"))
  tbls_right <- test_load(df_right, ignore = c("df", "sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right natural join with coalesce = TRUE and partial match", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, coalesce = TRUE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 7L))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = c(1:4, 7), y = 1:5, w = 1:5)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full natural join with coalesce = TRUE and partial match", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, coalesce = TRUE) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equal(as.integer(res$x), c(1L, 2L, 3L, 4L, 5L, 7L))
    expect_equal(as.integer(res$y), c(1L, 2L, 3L, 4L, 5L, 5L))
  }

  df_left <- data_frame(x = 1:5, y = 1:5, z = 1:5)
  df_right <- data_frame(x = c(1:4, 7), y = 1:5, w = 1:5)
  # SQLite doesn't support right and full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})
