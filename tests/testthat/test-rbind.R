context("rbind")

df_var <- data.frame(
  l = c(T, F, F),
  i = c(1, 1, 2),
  d = Sys.Date() + c(1, 1, 2),
  f = factor(letters[c(1, 1, 2)]),
  n = c(1, 1, 2) + 0.5,
  t = Sys.time() + c(1, 1, 2),
  c = letters[c(1, 1, 2)],
  stringsAsFactors = FALSE
)

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
  skip("to be discussed")
  my_list <- list(list(x = 1, y = "a"), list(x = 2, y = "b"))
  res <- bind_rows(my_list)
  expect_equal(nrow(res), 2L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")

  res <- bind_rows(list(x = 1, y = "a"), list(x = 2, y = "b"))
  expect_equal(nrow(res), 2L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")
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

test_that("empty data frame are handled (#1346)", {
  res <- tibble() %>% bind_rows(tibble(x = "a"))
  expect_equal(nrow(res), 1L)
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

test_that("bind_rows() correctly handles consecutive NULLs (#4296)", {
  res <- list(
    a = tibble(expected_id = "a"),
    b = NULL,
    c = NULL,
    d = tibble(expected_id = "d"),
    c = NULL,
    e = tibble(expected_id = "e")
  ) %>%
    bind_rows(.id = "id")
  expect_equal(res$id, res$expected_id)
})
