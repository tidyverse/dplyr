context("as-data-frame")


# as.data.frame and as_data_frame -----------------------------------------

test_that("as.data.frame works for SQL sources", {
  lf1 <- memdb_frame(x = letters)
  out <- lf1 %>%
    as.data.frame()

  expect_equal(out, data.frame(x = letters, stringsAsFactors = FALSE))
})

test_that("as_data_frame works for SQL sources", {
  if (packageVersion("tibble") < "1.0-4")
    skip("need tibble 1.0-4 or later for this test")

  lf1 <- memdb_frame(x = letters)
  out <- lf1 %>%
    as_data_frame()

  expect_equal(out, data_frame(x = letters))
})

test_that("as.data.frame is unlimited", {
  x <- rep(1:2, formals(collect.tbl_sql)$n)
  lf1 <- memdb_frame(x = x)
  out <- lf1 %>%
    as.data.frame()

  expect_equal(out, data.frame(x = x))
})

test_that("as_data_frame is unlimited", {
  if (packageVersion("tibble") < "1.0-4")
    skip("need tibble 1.0-4 or later for this test")

  x <- rep(1:2, formals(collect.tbl_sql)$n)
  lf1 <- memdb_frame(x = x)
  out <- lf1 %>%
    as_data_frame()

  expect_equal(out, data_frame(x = x))
})
