context("binds")


# error -------------------------------------------------------------------

test_that("bind_cols() err for non-data frames (#2373)", {
  skip("to be discussed")
  df1 <- tibble(x = 1)
  df2 <- structure(list(x = 1), class = "blah_frame")

  expect_error(
    bind_cols(df1, df2),
    "Argument 2 must be a data frame or a named atomic vector, not a blah_frame",
    fixed = TRUE
  )
})

test_that("bind_rows() err for non-data frames (#2373)", {
  skip("to be discussed")
  expect_error(
    bind_rows(df1, df2),
    "Argument 2 must be a data frame or a named atomic vector, not a blah_frame",
    fixed = TRUE
  )
})

test_that("bind_rows() err for invalid ID", {
  df1 <- tibble(x = 1:3)
  df2 <- tibble(x = 4:6)

  expect_error(
    bind_rows(df1, df2, .id = 5),
    "`.id` must be a scalar string, not a double vector of length 1",
    fixed = TRUE
  )
})


# columns -----------------------------------------------------------------

test_that("cbind uses shallow copies", {
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

  expect_equal(dfloc(df1), dfloc(df)[names(df1)])
  expect_equal(dfloc(df2), dfloc(df)[names(df2)])
})

test_that("bind_cols handles lists (#1104)", {
  exp <- tibble(x = 1, y = "a", z = 2)

  l1 <- list(x = 1, y = "a")
  l2 <- list(z = 2)

  expect_identical(bind_cols(l1, l2), exp)
  expect_identical(bind_cols(list(l1, l2)), exp)
})

test_that("bind_cols handles empty argument list (#1963)", {
  expect_equal(bind_cols(), data.frame())
})

test_that("bind_cols handles all-NULL values (#2303)", {
  expect_identical(bind_cols(list(a = NULL, b = NULL)), data.frame())
  expect_identical(bind_cols(NULL), data.frame())
})

test_that("bind_cols repairs names", {
  df <- tibble(a = 1, b = 2)
  bound <- bind_cols(df, df)

  repaired <- as_tibble(
    data.frame(a = 1, b = 2, a = 1, b = 2, check.names = FALSE),
    .name_repair = "unique"
  )

  expect_identical(bound, repaired)
})


# rows --------------------------------------------------------------------

df_var <- tibble(
  l = c(T, F, F),
  i = c(1, 1, 2),
  d = Sys.Date() + c(1, 1, 2),
  f = factor(letters[c(1, 1, 2)]),
  n = c(1, 1, 2) + 0.5,
  t = Sys.time() + c(1, 1, 2),
  c = letters[c(1, 1, 2)]
)

test_that("bind_rows() equivalent to rbind()", {
  exp <- tbl_df(rbind(df_var, df_var, df_var))
  res <- bind_rows(df_var, df_var, df_var)
  for(name in names(exp)) {
    expect_equal(res[[name]], exp[[name]])
  }
  res <- bind_rows(df_var, df_var, df_var)
  for(name in names(exp)) {
    expect_equal(res[[name]], exp[[name]])
  }
})

test_that("bind_rows reorders columns", {
  df_var_scramble <- df_var[sample(ncol(df_var))]

  expect_equal(
    names(bind_rows(df_var, df_var_scramble)),
    names(df_var)
  )
})

test_that("bind_rows ignores NULL", {
  df <- tibble(a = 1)

  expect_equal(bind_rows(df, NULL), df)
  expect_equal(bind_rows(list(df, NULL)), df)
})

test_that("bind_rows only accepts data frames or named vectors", {
  ll <- list(1:5, env(a = 1))
  expect_error(
    bind_rows(ll),
    "Argument 1 must have names",
    fixed = TRUE
  )
  ll <- list(tibble(a = 1:5), env(a = 1))
  expect_error(
    bind_rows(ll),
    "Argument 2 must be a data frame or a named atomic vector, not a environment",
    fixed = TRUE
  )
})

test_that("bind_rows handles list columns (#463)", {
  dfl <- tibble(x = list(1:2, 1:3, 1:4))
  res <- bind_rows(list(dfl, dfl))
  expect_equal(rep(dfl$x, 2L), res$x)
})

test_that("can bind lists of data frames #1389", {
  df <- tibble(x = 1)

  res <- bind_rows(list(df, df), list(df, df))
  expect_equal(nrow(res), 4)
})

test_that("bind_rows handles data frames with no rows (#597)", {
  df1 <- tibble(x = 1, y = factor("a"))
  df0 <- df1[0, ]

  expect_identical(bind_rows(df0), df0)
  expect_identical(bind_rows(df0, df0), df0)
  expect_identical(bind_rows(df0, df1), df1)
})

test_that("bind_rows handles data frames with no columns (#1346)", {
  df1 <- tibble(x = 1, y = factor("a"))
  df0 <- df1[, 0]

  expect_equal(bind_rows(df0), df0)
  expect_equal(dim(bind_rows(df0, df0)), c(2, 0))

  res <- bind_rows(df0, df1)
  expect_equal(res$x, c(NA, 1))
})

test_that("bind_rows handles lists with NULL values (#2056)", {
  df1 <- tibble(x = 1, y = 1)
  df2 <- tibble(x = 2, y = 2)
  lst1 <- list(a = df1, NULL, b = df2)

  df3 <- tibble(
    x = c(1, 2),
    y = c(1, 2),
    names = c("a", "b")
  )

  expect_identical(bind_rows(lst1, .id = "names"), df3)
})

test_that("bind_rows handles lists with list() values (#2826)", {
  expect_equal(bind_rows(list(iris, list())), iris)
})

test_that("bind_rows puts data frames in order received even if no columns (#2175)", {
  df2 <- tibble(x = 2, y = "b")
  df1 <- df2[, 0]

  res <- bind_rows(df1, df2)

  expect_equal(res$x, c(NA, 2))
  expect_equal(res$y, c(NA, "b"))
})

# Column coercion --------------------------------------------------------------

test_that("bind_rows promotes integer to numeric", {
  df1 <- tibble(a = 1L, b = 1L)
  df2 <- tibble(a = 1, b = 1L)

  res <- bind_rows(df1, df2)
  expect_equal(typeof(res$a), "double")
  expect_equal(typeof(res$b), "integer")
})

test_that("bind_rows promotes factor to character with warning", {
  df1 <- tibble(a = factor("a"))
  df2 <- tibble(a = "b")

  res <- bind_rows(df1, df2)
  expect_equal(typeof(res$a), "character")
})

test_that("bind_rows coerces factor when levels don't match", {
  df1 <- data.frame(a = factor("a"))
  df2 <- data.frame(a = factor("b"))

  res <- bind_rows(df1, df2)
  expect_equal(res$a, factor(c("a", "b")))
})

test_that("bind_rows handles NA in factors #279", {
  df1 <- tibble(a = factor("a"))
  df2 <- tibble(a = factor(NA))

  res <- bind_rows(df1, df2)
  expect_equal(res$a, factor(c("a", NA)))
})

test_that("bind_rows doesn't promote integer/numeric to factor", {
  df1 <- tibble(a = factor("a"))
  df2 <- tibble(a = 1L)
  df3 <- tibble(a = 1)

  expect_error(
    bind_rows(df1, df2),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    bind_rows(df1, df3),
    class = "vctrs_error_incompatible_type"
  )
})


test_that("bind_rows preserves timezones #298", {
  dates1 <- data.frame(
    ID = c("a", "b", "c"),
    dates = structure(c(-247320000, -246196800, -245073600),
      tzone = "GMT",
      class = c("POSIXct", "POSIXt")
    ),
    stringsAsFactors = FALSE
  )

  dates2 <- data.frame(
    ID = c("d", "e", "f"),
    dates = structure(c(-243864000, -242654400, -241444800),
      tzone = "GMT",
      class = c("POSIXct", "POSIXt")
    ),
    stringsAsFactors = FALSE
  )

  alldates <- bind_rows(dates1, dates2)
  expect_equal(attr(alldates$dates, "tzone"), "GMT")
})

test_that("bind_rows handles all NA columns (#493)", {
  mydata <- list(
    data.frame(x = c("foo", "bar")),
    data.frame(x = NA)
  )
  res <- bind_rows(mydata)
  expect_true(is.na(res$x[3]))
  expect_is(res$x, "factor")

  mydata <- list(
    data.frame(x = NA),
    data.frame(x = c("foo", "bar"))
  )
  res <- bind_rows(mydata)
  expect_true(is.na(res$x[1]))
  expect_is(res$x, "factor")
})

test_that("bind_rows handles complex. #933", {
  df1 <- data.frame(r = c(1 + 1i, 2 - 1i))
  df2 <- data.frame(r = c(1 - 1i, 2 + 1i))
  df3 <- bind_rows(df1, df2)
  expect_equal(nrow(df3), 4L)
  expect_equal(df3$r, c(df1$r, df2$r))
})

test_that("bind_rows is careful about column names encoding #1265", {
  one <- data.frame(foo = 1:3, bar = 1:3)
  names(one) <- c("f\u00fc", "bar")
  two <- data.frame(foo = 1:3, bar = 1:3)
  names(two) <- c("f\u00fc", "bar")
  Encoding(names(one)[1]) <- "UTF-8"
  expect_equal(names(one), names(two))
  res <- bind_rows(one, two)
  expect_equal(ncol(res), 2L)
})

test_that("bind_rows handles POSIXct (#1125)", {
  df1 <- data.frame(date = as.POSIXct(NA))
  df2 <- data.frame(date = as.POSIXct("2015-05-05"))
  res <- bind_rows(df1, df2)
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$date[1]))
})

test_that("bind_rows respects ordered factors (#1112)", {
  l <- c("a", "b", "c", "d")
  id <- factor(c("a", "c", "d"), levels = l, ordered = TRUE)
  df <- data.frame(id = rep(id, 2), val = rnorm(6))
  res <- bind_rows(df, df)
  expect_is(res$id, "ordered")
  expect_equal(levels(df$id), levels(res$id))

  res <- group_by(df, id) %>% do(na.omit(.))
  expect_is(res$id, "ordered")
  expect_equal(levels(df$id), levels(res$id))
})

test_that("bind_rows can handle lists (#1104)", {
  my_list <- list(tibble(x = 1, y = "a"), tibble(x = 2, y = "b"))
  res <- bind_rows(my_list)
  expect_equal(nrow(res), 2L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")
})

test_that("bind_rows can handle lists (#1104)", {
  skip("to be discussed")
  res <- bind_rows(list(x = 1, y = "a"), list(x = 2, y = "b"))
  expect_equal(nrow(res), 2L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")
})

test_that("bind_rows keeps ordered factors (#948)", {
  y <- bind_rows(
    data.frame(x = factor(c(1, 2, 3), ordered = TRUE)),
    data.frame(x = factor(c(1, 2, 3), ordered = TRUE))
  )
  expect_is(y$x, "ordered")
  expect_equal(levels(y$x), as.character(1:3))
})

test_that("bind handles POSIXct of different tz ", {
  date1 <- structure(-1735660800, tzone = "America/Chicago", class = c("POSIXct", "POSIXt"))
  date2 <- structure(-1735660800, tzone = "UTC", class = c("POSIXct", "POSIXt"))
  date3 <- structure(-1735660800, class = c("POSIXct", "POSIXt"))

  df1 <- data.frame(date = date1)
  df2 <- data.frame(date = date2)
  df3 <- data.frame(date = date3)

  res <- bind_rows(df1, df2)
  expect_equal(attr(res$date, "tzone"), "America/Chicago")

  res <- bind_rows(df1, df3)
  expect_equal(attr(res$date, "tzone"), "America/Chicago")

  res <- bind_rows(df2, df3)
  expect_equal(attr(res$date, "tzone"), "UTC")

  res <- bind_rows(df3, df3)
  expect_equal(attr(res$date, "tzone"), "")

  res <- bind_rows(df1, df2, df3)
  expect_equal(attr(res$date, "tzone"), "America/Chicago")
})

test_that("bind_rows() creates a column of identifiers (#1337)", {
  data1 <- mtcars[c(2, 3), ]
  data2 <- mtcars[1, ]

  out <- bind_rows(data1, data2, .id = "col")
  out_list <- bind_rows(list(data1, data2), .id = "col")
  expect_equal(names(out)[ncol(out)], "col")
  expect_equal(out$col, c("1", "1", "2"))
  expect_equal(out_list$col, c("1", "1", "2"))

  out_labelled <- bind_rows(one = data1, two = data2, .id = "col")
  out_list_labelled <- bind_rows(list(one = data1, two = data2), .id = "col")
  expect_equal(out_labelled$col, c("one", "one", "two"))
  expect_equal(out_list_labelled$col, c("one", "one", "two"))
})


test_that("string vectors are filled with NA not blanks before collection (#595)", {
  one <- mtcars[1:10, -10]
  two <- mtcars[11:32, ]
  two$char_col <- letters[1:22]

  res <- bind_rows(one, two)
  expect_true(all(is.na(res$char_col[1:10])))
})

test_that("bind_rows handles POSIXct stored as integer (#1402)", {
  now <- Sys.time()

  df1 <- data.frame(time = now)
  expect_equal(class(bind_rows(df1)$time), c("POSIXct", "POSIXt"))

  df2 <- data.frame(time = seq(now, length.out = 1, by = 1))
  expect_equal(class(bind_rows(df2)$time), c("POSIXct", "POSIXt"))

  res <- bind_rows(df1, df2)
  expect_equal(class(res$time), c("POSIXct", "POSIXt"))
  expect_true(all(res$time == c(df1$time, df2$time)))
})

test_that("bind_cols accepts NULL (#1148)", {
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

test_that("bind_rows handles 0-length named list (#1515)", {
  res <- bind_rows(list(a = 1)[-1])
  expect_equal(nrow(res), 0L)
  expect_is(res, "data.frame")
  expect_equal(ncol(res), 0L)
})

test_that("bind_rows handles promotion to strings (#1538)", {
  df1 <- tibble(b = c(1, 2))
  df2 <- tibble(b = c(1L, 2L))
  df3 <- tibble(b = factor(c("A", "B")))
  df4 <- tibble(b = c("C", "D"))

  expect_error(
    bind_rows(df1, df3),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    bind_rows(df1, df4),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    bind_rows(df2, df3),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    bind_rows(df2, df4),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("bind_rows infers classes from first result (#1692)", {
  d1 <- data.frame(a = 1:10, b = rep(1:2, each = 5))
  d2 <- tbl_df(d1)
  d3 <- group_by(d1, b)
  d4 <- rowwise(d1)
  d5 <- list(a = 1:10, b = rep(1:2, each = 5))

  expect_equal(class(bind_rows(d1, d1)), "data.frame")
  expect_equal(class(bind_rows(d2, d1)), c("tbl_df", "tbl", "data.frame"))
  res3 <- bind_rows(d3, d1)
  expect_equal(class(res3), c("grouped_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(map_int(group_rows(res3), length), c(10, 10))
  expect_equal(class(bind_rows(d4, d1)), c("rowwise_df", "tbl_df", "tbl", "data.frame"))
})

test_that("bind_rows infers classes from first result (#1692)", {
  skip("to be discussed")
  expect_equal(class(bind_rows(d5, d1)), c("tbl_df", "tbl", "data.frame"))
})

test_that("bind_cols infers classes from first result (#1692)", {
  d1 <- data.frame(a = 1:10, b = rep(1:2, each = 5))
  d2 <- tibble(c = 1:10, d = rep(1:2, each = 5))
  d3 <- group_by(d2, d)
  d4 <- rowwise(d2)
  d5 <- list(c = 1:10, d = rep(1:2, each = 5))

  expect_equal(class(bind_cols(d1, d1)), "data.frame")
  expect_equal(class(bind_cols(d2, d1)), c("tbl_df", "tbl", "data.frame"))
  res3 <- bind_cols(d3, d1)
  expect_equal(class(res3), c("grouped_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(map_int(group_rows(res3), length), c(5, 5))
  expect_equal(class(bind_cols(d4, d1)), c("rowwise_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(class(bind_cols(d5, d1)), c("tbl_df", "tbl", "data.frame"))
})

test_that("bind_rows rejects POSIXlt columns (#1789)", {
  df <- tibble(x = Sys.time() + 1:12)
  df$y <- as.POSIXlt(df$x)
  expect_error(
    bind_rows(df, df),
    "Argument 2 can't be a list containing POSIXlt values",
    fixed = TRUE
  )
})

test_that("bind_rows rejects data frame columns (#2015)", {
  df <- list(
    x = 1:10,
    y = data.frame(a = 1:10, y = 1:10)
  )
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(10)

  expect_error(
    dplyr::bind_rows(df, df),
    "Argument 2 can't be a list containing data frames",
    fixed = TRUE
  )
})

test_that("bind_rows accepts difftime objects", {
  df1 <- data.frame(x = as.difftime(1, units = "hours"))
  df2 <- data.frame(x = as.difftime(1, units = "mins"))
  res <- bind_rows(df1, df2)
  expect_equal(res$x, as.difftime(c(3600, 60), units = "secs"))
})

test_that("bind_rows accepts hms objects", {
  skip("until hms has better vctrs support")
  df1 <- data.frame(x = hms::hms(hours = 1))
  df2 <- data.frame(x = as.difftime(1, units = "mins"))
  res <- bind_rows(df1, df2)
  expect_equal(res$x, hms::hms(hours = c(1, 0), minutes = c(0, 1)))
})

test_that("bind_rows() fails with unnamed vectors", {
  expect_error(
    bind_rows(1:2),
    "Argument 1 must have names",
    fixed = TRUE
  )
})

test_that("bind_rows() handles rowwise vectors", {
  tbl <- bind_rows(
      tibble(a = "foo", b = "bar"),
      c(a = "A", b = "B"),
      set_names(factor(c("B", "B")), c("a", "b"))
    )
  expect_identical(tbl, tibble(a = c("foo", "A", "B"), b = c("bar", "B", "B")))

  id_tbl <- bind_rows(a = c(a = 1, b = 2), b = c(a = 3, b = 4), .id = "id")
  expect_equivalent(id_tbl, tibble(a = c(1, 3), b = c(2, 4), id = c("a", "b")))
})

test_that("bind_rows() accepts lists of dataframe-like lists as first argument", {
  skip("to be discussed")
  expect_identical(
    bind_rows(list(list(a = 1, b = 2))),
    tibble(a = 1, b = 2)
  )
})

test_that("columns that are OBJECT but have NULL class are handled gracefully (#3349)", {
  skip("until https://github.com/r-lib/vctrs/issues/563")
  mod <- lm(y ~ ., data = freeny)
  data <- model.frame(mod)
  data_list <- list(data, data)
  res <- bind_rows(data_list)
  expect_equal(names(res), names(data))
})

# Vectors ------------------------------------------------------------

test_that("accepts named columns", {
  expect_identical(bind_cols(a = 1:2, b = 3:4), tibble(a = 1:2, b = 3:4))
  expect_identical(bind_cols(!!!mtcars), as_tibble(mtcars))
})

test_that("uncompatible sizes fail", {
  expect_error(
    bind_cols(a = 1, mtcars),
    "Argument 2 must be length 1, not 32",
    fixed = TRUE
  )
  expect_error(
    bind_cols(mtcars, a = 1:3),
    "Argument 2 must be length 32, not 3",
    fixed = TRUE
  )
})

test_that("unnamed vectors fail", {
  expect_error(
    bind_cols(1:2),
    "Argument 1 must have names",
    fixed = TRUE
  )
  expect_error(
    bind_cols(!!!list(1:2)),
    "Argument 1 must have names",
    fixed = TRUE
  )
})

test_that("supports NULL values", {
  expect_identical(bind_cols(a = 1, NULL, b = 2, NULL), tibble(a = 1, b = 2))
})

test_that("bind_cols handles unnamed list (#3402)", {
  expect_identical(
    bind_cols(list(1, 2)),
    bind_cols(list(V1 = 1, V2 = 2))
  )
})

test_that("bind_rows handles typed lists (#3924)", {
  df <- data.frame(x = 1, y = 2)
  lst <- structure(list(df, df, df), class = "special_lst")
  expect_equal(bind_rows(lst), bind_rows(df,df,df))
})
