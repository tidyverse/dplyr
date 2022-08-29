test_that("bind_cols() uses shallow copies", {
  df1 <- data.frame(
    int = 1:10,
    num = rnorm(10),
    cha = letters[1:10],
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    log = sample(c(T, F), 10, replace = TRUE),
    dat = seq.Date(Sys.Date(), length.out = 10, by = "day"),
    tim = seq(Sys.time(), length.out = 10, by = "1 hour")
  )
  df <- bind_cols(df1, df2)

  expect_equal(lobstr::obj_addrs(df1), lobstr::obj_addrs(df[names(df1)]))
  expect_equal(lobstr::obj_addrs(df2), lobstr::obj_addrs(df[names(df2)]))
})

test_that("bind_cols() handles lists (#1104)", {
  exp <- tibble(x = 1, y = "a", z = 2)

  l1 <- list(x = 1, y = "a")
  l2 <- list(z = 2)

  expect_identical(bind_cols(l1, l2), exp)
  expect_identical(bind_cols(list(l1, l2)), exp)
})

test_that("bind_cols() handles empty argument list (#1963)", {
  expect_equal(bind_cols(), tibble())
})

test_that("bind_cols() handles all-NULL values (#2303)", {
  expect_identical(bind_cols(list(a = NULL, b = NULL)), tibble())
  expect_identical(bind_cols(NULL), tibble())
})

test_that("bind_cols() repairs names", {
  df <- tibble(a = 1, b = 2)
  expect_snapshot(bound <- bind_cols(df, df))

  expect_message(
    repaired <- as_tibble(
      data.frame(a = 1, b = 2, a = 1, b = 2, check.names = FALSE),
      .name_repair = "unique"
    ), "New names"
  )

  expect_identical(bound, repaired)
})

test_that("bind_cols() unpacks tibbles", {
  expect_equal(
    bind_cols(list(y = tibble(x = 1:2))),
    tibble(x = 1:2)
  )
  expect_equal(
    bind_cols(list(y = tibble(x = 1:2), z = tibble(y = 1:2))),
    tibble(x = 1:2, y = 1:2)
  )
})

test_that("bind_cols() honours .name_repair=", {
  expect_message(res <- bind_cols(
    data.frame(a = 1), data.frame(a = 2)
  ))
  expect_equal(res, data.frame(a...1 = 1, a...2 = 2))

  expect_error(bind_cols(.name_repair = "check_unique",
    data.frame(a = 1), data.frame(a = 2)
  ))
})

test_that("bind_cols() accepts NULL (#1148)", {
  df1 <- tibble(a = 1:10, b = 1:10)
  df2 <- tibble(c = 1:10, d = 1:10)

  res1 <- bind_cols(df1, df2)
  res2 <- bind_cols(NULL, df1, df2)
  res3 <- bind_cols(df1, NULL, df2)
  res4 <- bind_cols(df1, df2, NULL)

  expect_identical(res1, res2)
  expect_identical(res1, res3)
  expect_identical(res1, res4)
})

test_that("bind_cols() infers classes from first result (#1692)", {
  d1 <- data.frame(a = 1:10, b = rep(1:2, each = 5))
  d2 <- tibble(c = 1:10, d = rep(1:2, each = 5))
  d3 <- group_by(d2, d)
  d4 <- rowwise(d2)
  d5 <- list(c = 1:10, d = rep(1:2, each = 5))

  suppressMessages({
    expect_equal(class(bind_cols(d1, d1)), "data.frame")
    expect_equal(class(bind_cols(d2, d1)), c("tbl_df", "tbl", "data.frame"))
  })
  res3 <- bind_cols(d3, d1)
  expect_equal(class(res3), c("grouped_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(map_int(group_rows(res3), length), c(5, 5))
  expect_equal(class(bind_cols(d4, d1)), c("rowwise_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(class(bind_cols(d5, d1)), "data.frame")
})

test_that("accepts named columns", {
  expect_identical(bind_cols(a = 1:2, b = 3:4), tibble(a = 1:2, b = 3:4))
})

test_that("ignores NULL values", {
  expect_identical(bind_cols(a = 1, NULL, b = 2, NULL), tibble(a = 1, b = 2))
})

test_that("bind_cols() handles unnamed list with name repair (#3402)", {
  expect_snapshot(df <- bind_cols(list(1, 2)))

  expect_identical(df, bind_cols(list(...1 = 1, ...2 = 2)))
})

test_that("bind_cols() doesn't squash record types", {
  df <- data.frame(x = 1)
  posixlt <- as.POSIXlt(as.Date("1970-01-01"))

  expect_identical(
    bind_cols(df, y = posixlt),
    new_data_frame(list(x = 1, y = posixlt))
  )
})

test_that("bind_cols() gives informative errors", {
  expect_snapshot({
    "# incompatible size"
    (expect_error(bind_cols(a = 1:2, mtcars)))
    (expect_error(bind_cols(mtcars, a = 1:3)))
  })
})
