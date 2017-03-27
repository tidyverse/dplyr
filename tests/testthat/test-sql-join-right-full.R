context("SQL: right and full joins")

test_that("right join on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = "x") %>%
      arrange(x, y, z) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                 data.frame(
                   x = c(1L, 2L, 3L, 5L),
                   y = c(1L, 2L, 3L, NA),
                   z = c(1L, 2L, 3L, 4L)
                 ))
  }

  df_left <- data_frame(x = 1L:4L, y = 1L:4L)
  df_right <- data_frame(x = c(1L:3L, 5L), z = 1L:4L)
  # SQLite doesn't support right joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join on key column with same name in both tables", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, by = "x") %>%
      arrange(x, y, z) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                      data.frame(
                        x = c(1L, 2L, 3L, 4L, 5L),
                        y = c(1L, 2L, 3L, 4L, NA),
                        z = c(1L, 2L, 3L, NA, 4L)
                      ))
  }

  df_left <- data_frame(x = 1L:4L, y = 1L:4L)
  df_right <- data_frame(x = c(1L:3L, 5L), z = 1L:4L)
  # SQLite and MySQL don't support full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite", "mysql"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right join on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                      data.frame(
                        xl = c(1L, 2L, 3L, 5L),
                        y = c(1L, 2L, 3L, NA),
                        z = c(1L, 2L, 3L, 4L)
                      ))
  }

  df_left <- data_frame(xl = 1L:4L, y = 1L:4L)
  df_right <- data_frame(xr = c(1L:3L, 5L), z = 1L:4L)
  # SQLite doesn't support right joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full join on key column with different names", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right, by = c("xl" = "xr")) %>%
      arrange(xl, y, z) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                      data.frame(
                        xl = c(1L, 2L, 3L, 4L, 5L),
                        y = c(1L, 2L, 3L, 4L, NA),
                        z = c(1L, 2L, 3L, NA, 4L)
                      ))
  }

  df_left <- data_frame(xl = 1L:4L, y = 1L:4L)
  df_right <- data_frame(xr = c(1L:3L, 5L), z = 1L:4L)
  # SQLite and MySQL don't support full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite", "mysql"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("right natural join", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      right_join(tbl_left, tbl_right) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                      data.frame(
                        x = c(1L, 2L, 3L, 5L),
                        y = c(1L, 2L, 3L, 4L),
                        z = c(1L, 2L, 3L, NA),
                        w = c(1L, 2L, 3L, 4L)
                      ))
  }

  df_left <- data_frame(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  df_right <- data_frame(x = c(1L:3L, 5L), y=1L:4L, z = 1L:4L)
  # SQLite doesn't support right joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite"))
  mapply(test_f, tbls_left, tbls_right)
})

test_that("full natural join", {
  test_f <- function(tbl_left, tbl_right) {
    res <-
      full_join(tbl_left, tbl_right) %>%
      arrange(x, y, z, w) %>%
      collect()
    expect_equivalent(as.data.frame(res),
                      data.frame(
                        x = c(1L, 2L, 3L, 4L, 5L),
                        y = c(1L, 2L, 3L, 4L, 4L),
                        z = c(1L, 2L, 3L, 4L, NA),
                        w = c(1L, 2L, 3L, NA, 4L)
                      ))
  }

  df_left <- data_frame(x = 1L:4L, y = 1L:4L, w = 1L:4L)
  df_right <- data_frame(x = c(1L:3L, 5L), y=1L:4L, z = 1L:4L)
  # SQLite and MySQL don't support full joins
  tbls_left <- test_load(df_left, ignore = c("sqlite"))
  tbls_right <- test_load(df_right, ignore = c("sqlite", "mysql"))
  mapply(test_f, tbls_left, tbls_right)
})
