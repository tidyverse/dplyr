test_that("bind_rows() handles simple inputs", {
  df1 <- tibble(x = 1:2, y = letters[1:2])
  df2 <- tibble(x = 3:4, y = letters[3:4])

  out <- bind_rows(df1, df2)
  expect_equal(out, tibble(x = 1:4, y = letters[1:4]))
})

test_that("bind_rows() reorders columns to match first df", {
  df1 <- tibble(x = 1, y = 2)
  df2 <- tibble(y = 1, x = 2)

  expect_named(bind_rows(df1, df2), c("x", "y"))
})

test_that("bind_rows() returns union of columns", {
  df1 <- tibble(x = 1)
  df2 <- tibble(y = 2)
  out <- bind_rows(df1, df2)

  expect_equal(out, tibble(x = c(1, NA), y = c(NA, 2)))
})

test_that("bind_rows() handles zero column data frames (#2175)", {
  df1 <- tibble(.rows = 1)
  df2 <- tibble(x = 1)
  out <- bind_rows(df1, df2)

  expect_equal(out, tibble(x = c(NA, 1)))
})

test_that("bind_rows() handles zero row data frames (#597)", {
  df1 <- tibble(x = numeric())
  df2 <- tibble(y = 1)

  out <- bind_rows(df1, df2)
  expect_equal(out, tibble(x = NA_real_, y = 1))
})

test_that("bind_rows() ignores NULL (#2056)", {
  df <- tibble(a = 1)

  expect_equal(bind_rows(df, NULL), df)
  expect_equal(bind_rows(list(df, NULL)), df)
})

test_that("bind_rows() creates a column of identifiers (#1337)", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(x = 3)

  # with
  out <- bind_rows(a = df1, b = df2, .id = "id")
  expect_equal(out, tibble(id = c("a", "a", "b"), x = 1:3))

  out <- bind_rows(list(a = df1, b = df2), .id = "id")
  expect_equal(out, tibble(id = c("a", "a", "b"), x = 1:3))

  # or without names
  out <- bind_rows(df1, df2, .id = "id")
  expect_equal(out, tibble(id = c("1", "1", "2"), x = 1:3))
})

test_that("bind_rows deduplicates row names", {
  df1 <- data.frame(x = 1:2, row.names = c("a", "b"))
  df2 <- data.frame(x = 3:4, row.names = c("a", "c"))
  out <- bind_rows(df1, df2)

  expect_equal(rownames(out), c("a...1", "b", "a...3", "c"))
})

test_that("bind_rows respects the drop attribute of grouped df",{
  df <- tibble(
    e = 1,
    f = factor(c(1, 1, 2, 2), levels = 1:3),
    g = c(1, 1, 2, 2),
    x = c(1, 2, 1, 4)
  )
  df <- group_by(df, e, f, g, .drop = FALSE)

  gg <- bind_rows(df, df)
  expect_equal(group_size(gg), c(4L,4L,0L))
})

# bind_rows() magic ---------------------------------------------------

test_that("bind_rows() handles lists of data frames #1389", {
  df <- tibble(x = 1)

  res <- bind_rows(list(df, df), list(df, df))
  expect_equal(nrow(res), 4)
})

test_that("bind_rows() ignores empty lists (#2826)", {
  df <- tibble(x = 1:10)
  expect_equal(bind_rows(list(df, list())), df)
})

test_that("bind_rows() accepts lists of dataframe-like lists as first argument", {
  ll <- list(a = 1, b = 2)
  expect_equal(bind_rows(list(ll)), tibble(a = 1, b = 2))
  expect_equal(bind_rows(list(ll, ll)), tibble(a = c(1, 1), b = c(2, 2)))
})

test_that("bind_rows() can handle lists (#1104)", {
  ll <- list(list(x = 1, y = "a"), list(x = 2, y = "b"))
  out <- bind_rows(ll)
  expect_equal(out, tibble(x = c(1, 2), y = c("a", "b")))

  out <- bind_rows(ll[[1]], ll[2])
  expect_equal(out, tibble(x = c(1, 2), y = c("a", "b")))
})

test_that("bind_rows() handles 0-length named list (#1515)", {
  x <- set_names(list())
  expect_equal(bind_rows(x), tibble())
})

test_that("bind_rows() handles tibbles + vectors", {
  out <- bind_rows(
    tibble(a = 1, b = 2),
    c(a = 3, b = 4)
  )
  expect_equal(out, tibble(a = c(1, 3), b = c(2, 4)))

  out <- bind_rows(
    a = c(a = 1, b = 2),
    b = c(a = 3, b = 4),
    .id = "id"
  )
  expect_equal(out, tibble(id = c("a", "b"), a = c(1, 3), b = c(2, 4)))
})

test_that("bind_rows() only flattens S3 lists that inherit from list (#3924)", {
  df <- data.frame(x = 1, y = 2)

  lst1 <- structure(list(df, df, df), class = "special_lst")
  expect_snapshot(bind_rows(lst1), error = TRUE)

  lst2 <- structure(list(df, df, df), class = c("special_lst", "list"))
  expect_equal(bind_rows(lst2), bind_rows(df,df,df))
})

test_that("bind_rows() handles named list", {
  x <- list(x = 1, y = 2, z = 3)
  expect_equal(bind_rows(x), tibble(x = 1, y = 2, z = 3))
})

test_that("bind_rows() validates lists (#5417)", {
  out <- bind_rows(list(x = 1), list(x = 1, y = 1:2))
  expect_equal(out, tibble(x = c(1, 1, 1), y = c(NA, 1:2)))

  expect_snapshot(bind_rows(list(x = 1), list(x = 1:3, y = 1:2)), error = TRUE)
})

test_that("bind_rows() handles missing, null, and empty elements (#5429)", {
  x <- list(a = NULL, b = NULL)
  y <- list(a = "B", b = 2)
  l <- list(x, y)
  expect_identical(
    bind_rows(l),
    tibble(a = "B", b = 2)
  )

  x <- list(a = NULL, b = 1)
  y <- list(a = "B", b = 2)
  l <- list(x, y)
  expect_identical(
    bind_rows(l),
    tibble(b = c(1, 2), a = c(NA, "B"))
  )

  x <- list(a = character(0), b = 1)
  y <- list(a = "B", b = 2)
  l <- list(x, y)
  expect_identical(
    bind_rows(l),
    tibble(a = "B", b = 2)
  )
})

test_that("bind_rows(.id= NULL) does not set names (#5089)", {
  out <- bind_rows(list(a = tibble(x = 1:2)))
  expect_equal(attr(out, "row.names"), 1:2)

  out <- bind_rows(x = c(a = 1))
  expect_identical(attr(out, "row.names"), 1L)
})

# Column coercion --------------------------------------------------------------

test_that("bind_rows() promotes integer to numeric", {
  df1 <- tibble(a = 1L, b = 1L)
  df2 <- tibble(a = 1, b = 1L)

  res <- bind_rows(df1, df2)
  expect_type(res$a, "double")
  expect_type(res$b, "integer")
})

test_that("bind_rows() coerces factor when levels don't match", {
  df1 <- data.frame(a = factor("a"))
  df2 <- data.frame(a = factor("b"))

  res <- bind_rows(df1, df2)
  expect_equal(res$a, factor(c("a", "b")))
})

test_that("bind_rows() handles complex. #933", {
  df1 <- tibble(x = 1 + 1i)
  df2 <- tibble(x = 2 + 1i)

  out <- bind_rows(df1, df2)
  expect_equal(out, tibble(x = c(1 + 1i, 2 + 1i)))
})

test_that("bind_rows() handles POSIXct (#1125)", {
  df1 <- data.frame(date = as.POSIXct(NA))
  df2 <- data.frame(date = as.POSIXct("2015-05-05"))
  res <- bind_rows(df1, df2)
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$date[1]))
})

test_that("bind_rows() accepts data frame columns (#2015)", {
  df1 <- tibble(x = 1, y = tibble(a = 1, b = 1))
  df2 <- tibble(x = 2, y = tibble(a = 2, b = 2))

  out <- bind_rows(df1, df2)
  expect_equal(out, tibble(x = 1:2, y = tibble(a = 1:2, b = 1:2)))
})

test_that("bind_rows() accepts difftime objects", {
  df1 <- data.frame(x = as.difftime(1, units = "hours"))
  df2 <- data.frame(x = as.difftime(1, units = "mins"))
  res <- bind_rows(df1, df2)
  expect_equal(res$x, as.difftime(c(3600, 60), units = "secs"))
})

# Errors ------------------------------------------------------------

test_that("bind_rows() give informative errors", {
  expect_snapshot({
    "invalid .id"
    df1 <- tibble(x = 1:3)
    df2 <- tibble(x = 4:6)
    (expect_error(bind_rows(df1, df2, .id = 5)))

    "invalid type"
    ll <- list(tibble(a = 1:5), env(a = 1))
    (expect_error(bind_rows(ll)))

    df1 <- tibble(a = factor("a"))
    df2 <- tibble(a = 1L)
    (expect_error(bind_rows(df1, df2)))

    "unnamed vectors"
    (expect_error(bind_rows(1:2)))
  })
})
