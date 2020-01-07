context("Summarise")

test_that("repeated outputs applied progressively", {
  df <- data.frame(x = 5)

  out <- summarise(df, x = mean(x), x = x + 1)
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 1)

  expect_equal(out$x, 6)
})

test_that("repeated outputs applied progressively (grouped_df)", {
  df <- data.frame(x = c(1, 1), y = 1:2)
  ds <- group_by(df, y)
  out <- summarise(ds, z = mean(x), z = z + 1)

  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 2)

  expect_equal(out$z, c(2L, 2L))
})


test_that("summarise peels off a single layer of grouping", {
  df <- tibble(x = rep(1:4, each = 4), y = rep(1:2, each = 8), z = runif(16))
  grouped <- df %>% group_by(x, y, z)
  expect_equal(group_vars(grouped), c("x", "y", "z"))
  expect_equal(group_vars(grouped %>% summarise(n = n())), c("x", "y"))
})

test_that("summarise can refer to variables that were just created (#138)", {
  res <- summarise(tbl_df(mtcars), cyl1 = mean(cyl), cyl2 = cyl1 + 1)
  expect_equal(res$cyl2, mean(mtcars$cyl) + 1)

  gmtcars <- group_by(tbl_df(mtcars), am)
  res <- summarise(gmtcars, cyl1 = mean(cyl), cyl2 = cyl1 + 1)
  res_direct <- summarise(gmtcars, cyl2 = mean(cyl) + 1)
  expect_equal(res$cyl2, res_direct$cyl2)
})

test_that("summarise can refer to factor variables that were just created (#2217)", {
  df <- tibble(a = 1:3) %>%
    group_by(a)
  res <- df %>%
    summarise(f = factor(if_else(a <= 1, "a", "b")), g = (f == "a"))
  expect_equal(
    res,
    tibble(a = 1:3, f = factor(c("a", "b", "b")), g = c(TRUE, FALSE, FALSE))
  )
})

test_that("summarise can to modify grouping variables", {
  df <- tibble(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2))
  gf <- group_by(df, a)

  out <- summarise(gf, a = min(a) + 1)
  expect_equal(out$a, c(2, 3))
})

test_that("summarise gives proper errors (#153)", {
  df <- tibble(
    x = 1,
    y = c(1, 2, 2),
    z = runif(3)
  )
  # expect_error(
  #   summarise(df, null = identity(NULL)),
  #   "Column `identity(NULL)` is of unsupported type NULL",
  #   fixed = TRUE
  # )
  # expect_error(
  #   summarise(df, z = log(z)),
  #   "Column `log(z)` must be length 1 (a summary value), not 3",
  #   fixed = TRUE
  # )
  # expect_error(
  #   summarise(df, y = y[1:2]),
  #   "Column `y[1:2]` must be length 1 (a summary value), not 2",
  #   fixed = TRUE
  # )
  expect_error(
    summarise(df, a = env(a = 1)),
    "Unsupported type"
  )

  gdf <- group_by(df, x, y)
  expect_error(
    summarise(gdf, null = identity(NULL)),
    "Unsupported type"
  )
  # expect_error(
  #   summarise(gdf, a = z),
  #   "Column `z` must be length 1 (a summary value), not 2",
  #   fixed = TRUE
  # )
  # expect_error(
  #   summarise(gdf, a = log(z)),
  #   "Column `log(z)` must be length 1 (a summary value), not 2",
  #   fixed = TRUE
  # )
  # expect_error(
  #   summarise(gdf, a = y[1:2]),
  #   "Column `y[1:2]` must be length 1 (a summary value), not 2",
  #   fixed = TRUE
  # )
  expect_error(
    summarise(gdf, b = env(a = 1)),
    "Unsupported type",
    fixed = TRUE
  )
})

test_that("summarise handles constants (#153)", {
  df <- data.frame(a = 1:4)
  today <- Sys.Date()
  now <- Sys.time()

  res <- summarise(
    tbl_df(df),
    int = 1L, num = 1, str = "foo", bool = TRUE, date = today, time = now
  )
  expect_equal(res$int,  1L)
  expect_equal(res$num,  1.0)
  expect_equal(res$str,  "foo")
  expect_equal(res$bool, TRUE)
  expect_equal(res$date, today)
  expect_equal(res$time, now)

  res <- summarise(
    group_by(df, a),
    int = 1L, num = 1, str = "foo", bool = TRUE, date = today, time = now
  )
  expect_equal(res$int,  rep(1L, 4))
  expect_equal(res$num,  rep(1.0, 4))
  expect_equal(res$str,  rep("foo", 4))
  expect_equal(res$bool, rep(TRUE, 4))
  expect_equal(res$date, rep(today, 4))
  expect_equal(res$time, rep(now, 4))
})

test_that("summarise handles passing ...", {
  df <- data.frame(x = 1:4)

  f <- function(...) {
    x1 <- 1
    f1 <- function(x) x
    summarise(df, ..., x1 = f1(x1))
  }
  g <- function(...) {
    x2 <- 2
    f(x2 = x2, ...)
  }
  h <- function(before = "before", ..., after = "after") {
    g(before = before, ..., after = after)
  }

  res <- h(x3 = 3)
  expect_equal(res$x1, 1)
  expect_equal(res$x2, 2)
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- tbl_df(df)
  res <- h(x3 = 3)
  expect_equal(res$x1, 1)
  expect_equal(res$x2, 2)
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- group_by(df, x)
  res <- h(x3 = 3)
  expect_equal(res$x1, rep(1, 4))
  expect_equal(res$x2, rep(2, 4))
  expect_equal(res$before, rep("before", 4))
  expect_equal(res$after, rep("after", 4))
})

test_that("summarise propagate attributes (#194)", {
  df <- data.frame(
    b = rep(1:2, 2),
    f = Sys.Date() + 1:4,
    g = Sys.time() + 1:4,
    stringsAsFactors = FALSE
  ) %>%
    group_by(b)

  min_ <- min
  res <- summarise(df,
    min_f  = min(f),
    max_f  = max(f),
    min_g  = min(g),
    max_g  = max(g),
    min__f = min_(f),
    min__g = min_(g)
  )

  expect_equal(class(res$min_f), "Date")
  expect_equal(class(res$max_f), "Date")
  expect_equal(class(res$min__f), "Date")

  expect_equal(class(res$min_g), c("POSIXct", "POSIXt"))
  expect_equal(class(res$max_g), c("POSIXct", "POSIXt"))
  expect_equal(class(res$min__g), c("POSIXct", "POSIXt"))
})

test_that("summarise allows names (#2675)", {
  data <- tibble(a = 1:3) %>% summarise(b = c("1" = a[[1]]))
  expect_equal(names(data$b), "1")

  data <- tibble(a = 1:3) %>% rowwise() %>% summarise(b = setNames(nm = a))
  expect_equal(names(data$b), c("1", "2", "3"))

  data <- tibble(a = c(1, 1, 2)) %>% group_by(a) %>% summarise(b = setNames(nm = a[[1]]))
  expect_equal(names(data$b), c("1", "2"))

  res <- data.frame(x = c(1:3), y = letters[1:3]) %>%
    group_by(y) %>%
    summarise(
      a = length(x),
      b = quantile(x, 0.5)
    )
  expect_equal(res$b, c("50%" = 1, "50%" = 2, "50%" = 3))
})

test_that("summarise fails on missing variables", {
  # error messages from rlang
  expect_error(summarise(mtcars, a = mean(notthear)))
})

test_that("summarise fails on missing variables when grouping (#2223)", {
  # error messages from rlang
  expect_error(summarise(group_by(mtcars, cyl), a = mean(notthear)))
})

test_that("n() does not accept arguments", {
  expect_error(
    summarise(group_by(mtcars, cyl), n(hp))
  )
})

test_that("hybrid nests correctly", {
  res <- group_by(mtcars, cyl) %>%
    summarise(a = if (n() > 10) 1 else 2)
  expect_equal(res$a, c(1, 2, 1))

  res <- mtcars %>% summarise(a = if (n() > 10) 1 else 2)
  expect_equal(res$a, 1)
})

test_that("hybrid min and max propagate attributes (#246)", {
  x <- data.frame(
    id = c(rep("a", 2), rep("b", 2)),
    date = as.POSIXct(c("2014-01-13", "2014-01-14", "2014-01-15", "2014-01-16"), tz = "GMT")
  )
  y <- x %>% group_by(id) %>% summarise(max_date = max(date), min_date = min(date))

  expect_true("tzone" %in% names(attributes(y$min_date)))
  expect_true("tzone" %in% names(attributes(y$max_date)))
})

test_that("summarise can use newly created variable more than once", {
  df <- data.frame(id = c(1, 1, 2, 2, 3, 3), a = 1:6) %>% group_by(id)
  for (i in 1:10) {
    res <- summarise(
      df,
      biggest = max(a),
      smallest = min(a),
      diff1 = biggest - smallest,
      diff2 = smallest - biggest
    )
    expect_equal(res$diff1, -res$diff2)
  }
})

test_that("summarise creates an empty data frame with one row when no parameters are used", {
  res <- summarise(mtcars)
  expect_equal(nrow(res), 1L)
})

test_that("summarise works with zero-row data frames", {
  res <- summarise(mtcars[0, ], n = n(), sum = sum(cyl), mean = mean(mpg), var = var(drat))
  expect_equal(res, data.frame(n = 0L, sum = 0, mean = NaN, var = NA_real_))
})

test_that("summarise works with zero-column data frames (#3071)", {
  res <- summarise(mtcars[0], n = n())
  expect_equal(res, data.frame(n = nrow(mtcars)))
})

test_that("integer overflow (#304)", {
  groups <- rep(c("A", "B"), each = 3)
  values <- rep(1e9, 6)
  dat <- data.frame(groups, X1 = as.integer(values), X2 = values)
  # now group and summarise
  res <- group_by(dat, groups) %>%
    summarise(sum_integer = sum(X1), sum_numeric = sum(X2))
  expect_equal(res$sum_numeric, rep(3e9, 2L))
})

# test_that("summarise checks outputs (#300)", {
#   expect_error(
#     summarise(mtcars, mpg, cyl),
#     "Column `mpg` must be length 1 (a summary value), not 32",
#     fixed = TRUE
#   )
#   expect_error(
#     summarise(mtcars, mpg + cyl),
#     "Column `mpg + cyl` must be length 1 (a summary value), not 32",
#     fixed = TRUE
#   )
# })

test_that("comment attribute is allowed (#346)", {
  test <- data.frame(A = c(1, 1, 0, 0), B = c(2, 2, 3, 3))
  comment(test$B) <- "2nd Var"
  res <- group_by(test, A)
  expect_equal(comment(res$B), "2nd Var")
})

test_that("AsIs class is allowed (#453)", {
  test <- data.frame(A = c(1, 1, 0, 0), B = I(c(2, 2, 3, 3)))
  res <- group_by(test, B)
  expect_equal(res$B, test$B)
})

test_that("na.rm is supported (#168)", {
  df <- data.frame(
    x = c(1:5, NA, 7:10),
    y = rep(1:2, each = 5),
    z = c(rnorm(5), NA, rnorm(4))
  )
  res <- df %>%
    group_by(y) %>%
    summarise(
      mean_x = mean(x, na.rm = FALSE),
      mean_z = mean(z, na.rm = FALSE),
      min_x = min(x, na.rm = FALSE),
      min_z = min(z, na.rm = FALSE)
    )
  expect_equal(res$mean_x[1], 3)
  expect_true(is.na(res$mean_x[2]))
  expect_equal(res$mean_z[1], mean(df$z[1:5]))
  expect_true(is.na(res$mean_z[2]))

  expect_equal(res$min_x[1], 1)
  expect_true(is.na(res$min_x[2]))
  expect_equal(res$min_z[1], min(df$z[1:5]))
  expect_true(is.na(res$min_z[2]))

  res <- df %>%
    group_by(y) %>%
    summarise(
      mean_x = mean(x, na.rm = TRUE),
      mean_z = mean(z, na.rm = TRUE),
      min_x = min(x, na.rm = TRUE),
      min_z = min(z, na.rm = TRUE)
    )
  expect_equal(res$mean_x[1], 3)
  expect_equal(res$mean_x[2], 8.5)
  expect_equal(res$mean_z[1], mean(df$z[1:5]))
  expect_equal(res$mean_z[2], mean(df$z[7:10]))

  expect_equal(res$min_x[1], 1)
  expect_equal(res$min_x[2], 7)
  expect_equal(res$min_z[1], min(df$z[1:5]))
  expect_equal(res$min_z[2], min(df$z[7:10]))
})

test_that("summarise hybrid functions can use summarized variables", {
  df <- data.frame(x = c(1:5, NA, 7:10), y = rep(1:2, each = 5)) %>% group_by(y)
  res <- summarise(
    df,
    x = mean(x),
    min = min(x),
    max = max(x),
    mean = mean(x),
    var = var(x)
  )
  expect_identical(res$x, res$min)
  expect_identical(res$x, res$max)
  expect_identical(res$x, res$mean)
  expect_identical(res$var, rep(NA_real_, 2))
})

test_that("LazySubset is not confused about input data size (#452)", {
  res <- data.frame(a = c(10, 100)) %>% summarise(b = sum(a), c = sum(a) * 2)
  expect_equal(res$b, 110)
  expect_equal(res$c, 220)
})

test_that("nth, first, last promote dates and times (#509)", {
  data <- tibble(
    ID = rep(letters[1:4], each = 5),
    date = Sys.Date() + 1:20,
    time = Sys.time() + 1:20,
    number = rnorm(20)
  )
  res <- data %>%
    group_by(ID) %>%
    summarise(
      date2 = nth(date, 2),
      time2 = nth(time, 2),
      first_date = first(date),
      last_date = last(date),
      first_time = first(time),
      last_time = last(time)
    )
  expect_is(res$date2, "Date")
  expect_is(res$first_date, "Date")
  expect_is(res$last_date, "Date")
  expect_is(res$time2, "POSIXct")
  expect_is(res$first_time, "POSIXct")
  expect_is(res$last_time, "POSIXct")
  # error messages from rlang
  expect_error(data %>% group_by(ID) %>% summarise(time2 = nth(times, 2)))
})

test_that("nth, first, last preserves factor data (#509)", {
  dat <- tibble(a = rep(seq(1, 20, 2), 3), b = as.ordered(a))
  dat1 <- dat %>%
    group_by(a) %>%
    summarise(
      der = nth(b, 2),
      first = first(b),
      last = last(b)
    )
  expect_is(dat1$der, "ordered")
  expect_is(dat1$first, "ordered")
  expect_is(dat1$last, "ordered")
  expect_equal(levels(dat1$der), levels(dat$b))
})

test_that("nth handle negative value (#1584) ", {
  df <- data.frame(
    a = 1:10, b = 10:1,
    g = rep(c(1, 2), c(4, 6))
  ) %>%
    group_by(g)

  res <- summarise(
    df,
    x1 = nth(a, -1L),
    x2 = nth(a, -1L, order_by = b),
    x3 = nth(a, -5L),
    x4 = nth(a, -5L, order_by = b),
    x5 = nth(a, -5L, default = 99),
    x6 = nth(a, -5L, order_by = b, default = 99)
  )
  expect_equal(res$x1, c(4, 10))
  expect_equal(res$x2, c(1, 5))
  expect_true(is.na(res$x3[1]))
  expect_equal(res$x3[2], 6)
  expect_true(is.na(res$x4[1]))
  expect_equal(res$x4[2], 9)
  expect_equal(res$x5, c(99, 6))
  expect_equal(res$x6, c(99, 9))
})

test_that("LazyGroupSubsets is robust about columns not from the data (#600)", {
  foo <- tibble(x = 1:10, y = 1:10)
  # error messages from rlang
  expect_error(foo %>% group_by(x) %>% summarise(first_y = first(z)))
})

test_that("can summarise first(x[-1]) (#1980)", {
  expect_equal(
    tibble(x = 1:3) %>% summarise(f = first(x[-1])),
    tibble(f = 2L)
  )
})

test_that("hybrid eval handles $ and @ (#645)", {
  tmp <- expand.grid(a = 1:3, b = 0:1, i = 1:10)
  g <- tmp %>% group_by(a)

  f <- function(a, b) {
    list(x = 1:10)
  }

  res <- g %>% summarise(
    r = sum(b),
    n = length(b),
    p = f(r, n)$x[1]
  )
  expect_equal(names(res), c("a", "r", "n", "p"))

  res <- tmp %>% summarise(
    r = sum(b),
    n = length(b),
    p = f(r, n)$x[1]
  )
  expect_equal(names(res), c("r", "n", "p"))
})

test_that("argument order_by in last is flexible enough to handle more than just a symbol (#626)", {
  res1 <- group_by(mtcars, cyl) %>%
    summarise(
      big = last(mpg[drat > 3], order_by = wt[drat > 3]),
      small = first(mpg[drat > 3], order_by = wt[drat > 3]),
      second = nth(mpg[drat > 3], 2, order_by = wt[drat > 3])
    )

  # turning off lazy eval
  last. <- last
  first. <- first
  nth. <- nth
  res2 <- group_by(mtcars, cyl) %>%
    summarise(
      big = last.(mpg[drat > 3], order_by = wt[drat > 3]),
      small = first.(mpg[drat > 3], order_by = wt[drat > 3]),
      second = nth.(mpg[drat > 3], 2, order_by = wt[drat > 3])
    )
  expect_equal(res1, res2)
})

test_that("min(., na.rm=TRUE) correctly handles Dates that are coded as REALSXP (#755)", {
  dates <- as.Date(c("2014-01-01", "2013-01-01"))
  dd <- data.frame(Dates = dates)
  res <- summarise(dd, Dates = min(Dates, na.rm = TRUE))
  expect_is(res$Dates, "Date")
  expect_equal(res$Dates, as.Date("2013-01-01"))
})

test_that("nth handles expressions for n argument (#734)", {
  df <- data.frame(x = c(1:4, 7:9, 13:19), y = sample(100:999, 14))
  idx <- which(df$x == 16)
  res <- df %>% summarize(abc = nth(y, n = which(x == 16)))
  expect_equal(res$abc, df$y[idx])
})

test_that("summarise is not polluted by logical NA (#599)", {
  dat <- data.frame(grp = rep(1:4, each = 2), val = c(NA, 2, 3:8))
  Mean <- function(x, thresh = 2) {
    res <- mean(x, na.rm = TRUE)
    if (res > thresh) res else NA
  }
  res <- dat %>% group_by(grp) %>% summarise(val = Mean(val, thresh = 2))
  expect_is(res$val, "numeric")
  expect_true(is.na(res$val[1]))
})

test_that("summarise handles list output columns (#832)", {
  df <- tibble(x = 1:10, g = rep(1:2, each = 5))
  res <- df %>% group_by(g) %>% summarise(y = list(x))
  expect_equal(res$y[[1]], 1:5)
  expect_equal(res$y[[2]], 6:10)

  res <- df %>% group_by(g) %>% summarise(y = list(x + 1))
  expect_equal(res$y[[1]], 1:5 + 1)
  expect_equal(res$y[[2]], 6:10 + 1)

  df <- tibble(x = 1:10, g = rep(1:2, each = 5))
  res <- df %>% summarise(y = list(x))
  expect_equal(res$y[[1]], 1:10)
  res <- df %>% summarise(y = list(x + 1))
  expect_equal(res$y[[1]], 1:10 + 1)
})

test_that("summarise works with empty data frame (#1142)", {
  df <- data.frame()
  res <- df %>% summarise()
  expect_equal(nrow(res), 1L)
  expect_equal(length(res), 0L)
})

test_that("n_distint uses na.rm argument", {
  df <- data.frame(x = c(1:3, NA), g = rep(1:2, 2))
  res <- summarise(df, n = n_distinct(x, na.rm = TRUE))
  expect_equal(res$n, 3L)

  res <- group_by(df, g) %>% summarise(n = n_distinct(x, na.rm = TRUE))
  expect_equal(res$n, c(2L, 1L))
})

test_that("n_distinct front end supports na.rm argument (#1052)", {
  x <- c(1:3, NA)
  expect_equal(n_distinct(x, na.rm = TRUE), 3L)
})

test_that("n_distinct() without arguments gives 0 (#1957)", {
  expect_equal(n_distinct(), 0)
})

test_that("hybrid evaluation does not take place for objects with a class (#1237)", {
  mean.foo <- function(x) 42
  df <- tibble(x = structure(1:10, class = "foo"))
  expect_equal(summarise(df, m = mean(x))$m[1], 42)

  env <- environment()
  Foo <- suppressWarnings(setClass("Foo", contains = "numeric", where = env))
  suppressMessages(setMethod("mean", "Foo", function(x, ...) 42, where = env))
  on.exit(removeClass("Foo", where = env))

  df <- data.frame(x = Foo(c(1, 2, 3)))
  expect_equal(summarise(df, m = mean(x))$m[1], 42)
})

test_that("summarise handles promotion of results (#893)", {
  df <- structure(
    list(
      price = c(
        580L, 650L, 630L, 706L, 1080L, 3082L, 3328L, 4229L, 1895L,
        3546L, 752L, 13003L, 814L, 6115L, 645L, 3749L, 2926L, 765L,
        1140L, 1158L
      ),
      cut = structure(c(
        2L, 4L, 4L, 2L, 3L, 2L, 2L, 3L, 4L, 1L, 1L, 3L, 2L,
        4L, 3L, 3L, 1L, 2L, 2L, 2L
      ),
      .Label = c("Good", "Ideal", "Premium", "Very Good"),
      class = "factor"
      )
    ),
    row.names = c(NA, -20L),
    .Names = c("price", "cut"),
    class = "data.frame"
  )
  res <- df %>%
    group_by(cut) %>%
    select(price) %>%
    summarise(price = median(price))
  expect_is(res$price, "numeric")
})

test_that("summarise correctly handles logical (#1291)", {
  test <- expand.grid(id = 1:2, type = letters[1:2], sample = 1:2) %>%
    mutate(var = c(1, 0, 1, 1, 0, 0, 0, 1)) %>%
    mutate(var_l = as.logical(var)) %>%
    mutate(var_ch = as.character(var_l)) %>%
    arrange(id, type, sample) %>%
    group_by(id, type)
  test_sum <- test %>%
    ungroup() %>%
    group_by(id, type) %>%
    summarise(
      anyvar = any(var == 1),
      anyvar_l = any(var_l),
      anyvar_ch = any(var_ch == "TRUE")
    )

  expect_equal(test_sum$anyvar, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("summarise correctly handles NA groups (#1261)", {
  tmp <- tibble(
    a = c(1, 1, 1, 2, 2),
    b1 = NA_integer_,
    b2 = NA_character_
  )

  res <- tmp %>% group_by(a, b1) %>% summarise(n = n())
  expect_equal(nrow(res), 2L)
  res <- tmp %>% group_by(a, b2) %>% summarise(n = n())
  expect_equal(nrow(res), 2L)
})

test_that("n_distinct handles multiple columns (#1084)", {
  df <- data.frame(
    x = rep(1:4, each = 2),
    y = rep(1:2, each = 4),
    g = rep(1:2, 4)
  )
  res <- summarise(df, n = n_distinct(x, y))
  expect_equal(res$n, 4L)

  res <- group_by(df, g) %>% summarise(n = n_distinct(x, y))
  expect_equal(res$n, c(4L, 4L))

  df$x[3] <- df$y[7] <- NA
  res <- summarise(df, n = n_distinct(x, y))
  expect_equal(res$n, 6L)
  res <- summarise(df, n = n_distinct(x, y, na.rm = TRUE))
  expect_equal(res$n, 4L)

  res <- group_by(df, g) %>% summarise(n = n_distinct(x, y))
  expect_equal(res$n, c(4L, 4L))

  res <- group_by(df, g) %>% summarise(n = n_distinct(x, y, na.rm = TRUE))
  expect_equal(res$n, c(2L, 4L))
})

test_that("hybrid max works when not used on columns (#1369)", {
  df <- tibble(x = 1:1000)
  y <- 1:10
  expect_equal(summarise(df, z = max(y))$z, 10)
  expect_equal(summarise(df, z = max(10))$z, 10)
})

test_that("min and max handle empty sets in summarise (#1481, #3997)", {
  df <- tibble(A = numeric())
  expect_warning(
    res <- df %>% summarise(Min = min(A, na.rm = TRUE), Max = max(A, na.rm = TRUE))
  )
  expect_equal(res$Min, Inf)
  expect_equal(res$Max, -Inf)
})

test_that("lead and lag behave correctly in summarise (#1434)", {
  res <- mtcars %>%
    summarise(
      n = n(),
      leadn = lead(n),
      lagn = lag(n),
      leadn10 = lead(n, default = 10),
      lagn10 = lag(n, default = 10)
    )
  expect_true(all(is.na(res$lagn)))
  expect_true(all(is.na(res$leadn)))
  expect_true(all(res$lagn10 == 10))
  expect_true(all(res$leadn10 == 10))

  res <- mtcars %>%
    group_by(cyl) %>%
    summarise(
      n = n(),
      leadn = lead(n),
      lagn = lag(n),
      leadn10 = lead(n, default = 10),
      lagn10 = lag(n, default = 10)
    )
  expect_true(all(is.na(res$lagn)))
  expect_true(all(is.na(res$leadn)))
  expect_true(all(res$lagn10 == 10))
  expect_true(all(res$leadn10 == 10))

  res <- mtcars %>%
    rowwise() %>%
    summarise(
      n = n(),
      leadn = lead(n),
      lagn = lag(n),
      leadn10 = lead(n, default = 10),
      lagn10 = lag(n, default = 10)
    )
  expect_true(all(is.na(res$lagn)))
  expect_true(all(is.na(res$leadn)))
  expect_true(all(res$lagn10 == 10))
  expect_true(all(res$leadn10 == 10))
})

# .data and .env tests now in test-hybrid-traverse.R

test_that("data.frame columns are supported in summarise (#1425)", {
  df <- data.frame(x1 = rep(1:3, times = 3), x2 = 1:9)
  df$x3 <- df %>% mutate(x3 = x2)
  res <- df %>% group_by(x1) %>% summarise(nr = nrow(x3))
  expect_true(all(res$nr == 3))
})

test_that("summarise handles min/max of already summarised variable (#1622)", {
  df <- data.frame(
    FIRST_DAY = rep(seq(as.POSIXct("2015-12-01", tz = "UTC"), length.out = 2, by = "days"), 2),
    event = c("a", "a", "b", "b")
  )

  df_summary <- df %>%
    group_by(event) %>%
    summarise(FIRST_DAY = min(FIRST_DAY), LAST_DAY = max(FIRST_DAY))
  expect_equal(df_summary$FIRST_DAY, df_summary$LAST_DAY)
})

test_that("summarise handles double summary (#3233)", {
  df <- data.frame(a = 1:3)

  df_summary <-
    df %>%
    summarise(b = sum(a), c = sum(b))

  expect_equal(df_summary, data.frame(b = 6, c = 6))

  df_summary <-
    df %>%
    summarise(b = mean(a), c = mean(b))

  expect_equal(df_summary, data.frame(b = 2, c = 2))

  df_summary <-
    df %>%
    summarise(b = var(a), c = var(b))

  expect_equal(df_summary, data.frame(b = 1, c = NA_real_))

  df_summary <-
    df %>%
    summarise(b = sd(a), c = sd(b))

  expect_equal(df_summary, data.frame(b = 1, c = NA_real_))
})

test_that("group_by keeps classes (#1631)", {
  df <- data.frame(a = 1, b = as.Date(NA)) %>%
    group_by(a) %>%
    summarize(c = min(b))
  expect_equal(class(df$c), "Date")

  df <- data.frame(a = 1, b = as.POSIXct(NA)) %>%
    group_by(a) %>%
    summarize(c = min(b))
  expect_equal(class(df$c), c("POSIXct", "POSIXt"))
})

test_that("hybrid n_distinct falls back to R evaluation when needed (#1657)", {
  dat3 <- data.frame(id = c(2, 6, 7, 10, 10))
  res <- dat3 %>% summarise(n_unique = n_distinct(id[id > 6]))
  expect_equal(res$n_unique, 2)
})

test_that("summarise() correctly coerces factors with different levels (#1678)", {
  res <- tibble(x = 1:3) %>%
    group_by(x) %>%
    summarise(
      y = if (x == 1) "a" else "b",
      z = factor(y)
    )
  expect_is(res$z, "factor")
  expect_equal(levels(res$z), c("a", "b"))
  expect_equal(as.character(res$z), c("a", "b", "b"))
})

test_that("summarise handles raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_equal(summarise(df, c = sum(a)), tibble(c = 6L))
  expect_identical(summarise(df, c = b[[1]]), tibble(c = as.raw(1)))
})

test_that("summarise supports matrix columns", {
  df <- data.frame(a = 1:3, b = 1:3)

  df_regular <- summarise(df, b = scale(b))
  df_grouped <- summarise(group_by(df, a), b = scale(b))
  df_rowwise <- summarise(rowwise(df), b = scale(b))

  expect_equal(dim(df_regular$b), c(3, 1))
  expect_equal(dim(df_grouped$b), c(3, 1))
  expect_equal(dim(df_rowwise$b), c(3, 1))
})

test_that("typing and NAs for grouped summarise (#1839)", {
  expect_identical(
    tibble(id = 1L, a = NA_character_) %>%
      group_by(id) %>%
      summarise(a = a[[1]]) %>%
      .$a,
    NA_character_
  )

  expect_identical(
    tibble(id = 1:2, a = c(NA, "a")) %>%
      group_by(id) %>%
      summarise(a = a[[1]]) %>%
      .$a,
    c(NA, "a")
  )

  # Properly upgrade NA (logical) to character
  expect_identical(
    tibble(id = 1:2, a = 1:2) %>%
      group_by(id) %>%
      summarise(a = ifelse(all(a < 2), NA, "yes")) %>%
      .$a,
    c(NA, "yes")
  )

  expect_error(
    tibble(id = 1:2, a = list(1, "2")) %>%
      group_by(id) %>%
      summarise(a = a[[1]]) %>%
      .$a,
    "No common type",
    class = "vctrs_error_incompatible_type"
  )

  expect_identical(
    tibble(id = 1:2, a = list(1, "2")) %>%
      group_by(id) %>%
      summarise(a = a[1]) %>%
      .$a,
    list(1, "2")
  )
})

test_that("typing and NAs for rowwise summarise (#1839)", {
  expect_identical(
    tibble(id = 1L, a = NA_character_) %>%
      rowwise() %>%
      summarise(a = a[[1]]) %>%
      .$a,
    NA_character_
  )

  expect_identical(
    tibble(id = 1:2, a = c(NA, "a")) %>%
      rowwise() %>%
      summarise(a = a[[1]]) %>%
      .$a,
    c(NA, "a")
  )

  # Properly promote NA (logical) to character
  expect_identical(
    tibble(id = 1:2, a = 1:2) %>%
      group_by(id) %>%
      summarise(a = ifelse(all(a < 2), NA, "yes")) %>%
      .$a,
    c(NA, "yes")
  )

  expect_error(
    tibble(id = 1:2, a = list(1, "2")) %>%
      rowwise() %>%
      summarise(a = a[[1]]) %>%
      .$a,
    "No common type",
    class = "vctrs_error_incompatible_type"
  )

  expect_error(
    tibble(id = 1:2, a = list(1, "2")) %>%
      rowwise() %>%
      summarise(a = a[1]) %>%
      .$a,
    "No common type",
    class = "vctrs_error_incompatible_type"
  )
})

test_that("calculating an ordered factor preserves order (#2200)", {
  test_df <- tibble(
    id = c("a", "b"),
    val = 1:2
  )

  ret <- group_by(test_df, id) %>%
    summarize(level = ordered(val))

  expect_s3_class(ret$level, "ordered")
  expect_equal(levels(ret$level), c("1", "2"))
})

test_that("min, max preserves ordered factor data  (#2200)", {
  test_df <- tibble(
    id = rep(c("a", "b"), 2),
    ord = ordered(c("A", "B", "B", "A"), levels = c("A", "B"))
  )

  ret <- group_by(test_df, id) %>%
    summarize(
      min_ord = min(ord),
      max_ord = max(ord)
    )

  expect_s3_class(ret$min_ord, "ordered")
  expect_s3_class(ret$max_ord, "ordered")
  expect_equal(levels(ret$min_ord), levels(test_df$ord))
  expect_equal(levels(ret$max_ord), levels(test_df$ord))
})

test_that("ungrouped summarise() uses summary variables correctly (#2404)", {
  df <- tibble(value = seq(1:10))

  out <- df %>% summarise(value = mean(value), sd = sd(value))
  expect_equal(out$value, 5.5)
  expect_equal(out$sd, NA_real_)
})

test_that("proper handling of names in summarised list columns (#2231)", {
  d <- tibble(x = rep(1:3, 1:3), y = 1:6, names = letters[1:6])
  res <- d %>% group_by(x) %>% summarise(y = list(setNames(y, names)))
  expect_equal(names(res$y[[1]]), letters[[1]])
  expect_equal(names(res$y[[2]]), letters[2:3])
  expect_equal(names(res$y[[3]]), letters[4:6])
})

test_that("proper handling of NA factors (#2588)", {
  df <- tibble(
    x = c(1, 1, 2, 2, 3, 3),
    y = factor(c(NA, NA, NA, "2", "3", "3"))
  )

  ret <- df %>% group_by(x) %>% summarise(y = y[1])
  expect_identical(as.character(ret$y), c(NA, NA, "3"))
})

test_that("can refer to previously summarised symbols", {
  expect_identical(summarise(group_by(mtcars, cyl), x = 1, z = x)[2:3], tibble(x = c(1, 1, 1), z = x))
  expect_identical(summarise(group_by(mtcars, cyl), x = n(), z = x)[2:3], tibble(x = c(11L, 7L, 14L), z = x))
})

test_that("can refer to symbols if group size is one overall", {
  df <- tibble(x = LETTERS[3:1], y = 1:3)
  expect_identical(
    df %>%
      group_by(x) %>%
      summarise(z = y),
    tibble(x = LETTERS[1:3], z = 3:1)
  )
})

test_that("summarise() supports unquoted values", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  expect_identical(summarise(df, out = !!1), tibble(out = 1))
  expect_identical(summarise(df, out = !!quote(identity(1))), tibble(out = 1))
  expect_equal(summarise(df, out = !!(1:2)), tibble(out = 1:2))
  expect_error(summarise(df, out = !!env(a = 1)), "Unsupported type")

  gdf <- group_by(df, g)
  expect_identical(summarise(gdf, out = !!1), summarise(gdf, out = 1))
  expect_identical(summarise(gdf, out = !!(1:2)), tibble(g = c(1, 1, 2, 2), out = c(1:2, 1:2)))
  expect_identical(summarise(gdf, out = !!quote(identity(1))), summarise(gdf, out = 1))
  expect_equal(summarise(gdf, out = !!(1:5)) %>% nrow(), 10L)
  expect_error(summarise(gdf, out = !!env(a = 1)), "Unsupported type")
})

test_that("hybrid sum handles NA correctly (#3528)",{
  d <- tibble(x = c(1L,2L,NA) )

  expect_equal( summarise(d, x = sum(x, na.rm = TRUE)), tibble(x = 3L))
  expect_equal( summarise(d, x = sum(x, na.rm = FALSE)), tibble(x = NA_integer_))
  expect_equal( summarise(d, x = sum(x)), tibble(x = NA_integer_))

  d <- tibble(x = c(TRUE, FALSE, NA) )
  expect_equal( summarise(d, x = sum(x, na.rm = TRUE)), tibble(x = 1L))
  expect_equal( summarise(d, x = sum(x, na.rm = FALSE)), tibble(x = NA_integer_))
  expect_equal( summarise(d, x = sum(x)), tibble(x = NA_integer_))
})

test_that("formulas are evaluated in the right environment (#3019)", {
  out <- mtcars %>% summarise(fn = list(rlang::as_function(~ list(~foo, environment()))))
  out <- out$fn[[1]]()
  expect_identical(environment(out[[1]]), out[[2]])
})

test_that("summarise correctly reconstruct group rows", {
  d <- tibble(x = 1:4, g1 = rep(1:2, 2), g2 = 1:4) %>%
    group_by(g1, g2) %>%
    summarise(x = x+1)
  expect_equal(group_rows(d), list_of(1:2, 3:4))
})

test_that("summarise can handle POSIXlt columns (#3854)", {
  df <- data.frame(g=c(1,1,3))
  df$created <- strptime(c("2014/1/1", "2014/1/2", "2014/1/2"), format = "%Y/%m/%d")

  res <- df %>%
    group_by(g) %>%
    summarise(data = list(created))
  expect_true(all(sapply(res$data, inherits, "POSIXlt")))
})

test_that("tidy eval does not infloop (#4049)", {
  df <- data.frame(x = 1:5)
  call <- expr(length(!!quo(x)))
  expect_identical(summarise(df, x = eval_tidy(call)), data.frame(x = 5L))
})

test_that("hybrid sum(), mean(), min(), max() treats NA and NaN correctly (#4108, #4163)", {
  res <- data.frame(x = c(1, NaN)) %>%
    summarise(sum = sum(x), mean = mean(x), max = max(x), min = min(x))
  expect_true(is.nan(res$sum))
  expect_true(is.nan(res$mean))
  expect_true(is.nan(res$max))
  expect_true(is.nan(res$min))

  res <- data.frame(x = c(1, NaN)) %>%
    summarise(
      sum = sum(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE)
    )
  expect_equal(res$sum , 1)
  expect_equal(res$mean, 1)
  expect_equal(res$max , 1)
  expect_equal(res$min , 1)
})

test_that("hybrid min() and max() coerce to integer if there is no infinity (#4258)", {
  tbl <- data.frame(a = 1L) %>% summarise_all(list(min = min, max = max))
  expect_equal(tbl, data.frame(min = 1L, max = 1L))
  expect_is(tbl$min, "integer")
  expect_is(tbl$max, "integer")

  expect_warning(
    tbl <- data.frame(a = 1L, b = factor("a", levels = c("a", "b"))) %>%
      group_by(b, .drop = FALSE) %>%
      summarise_all(list(min = min, max = max))
  )
  expect_equal(tbl,
    tibble(
      b = factor(c("a", "b"), levels = c("a", "b")),
      min = c(1, Inf),
      max = c(1, -Inf)
    )
  )
  expect_is(tbl$min, "numeric")
  expect_is(tbl$max, "numeric")
})

test_that("summarise() correctly handle summarised list columns (#4349)", {
  res <- tibble(grp = "grp") %>%
    group_by(grp) %>%
    summarise(z = list(1), y = z)
  expect_identical(res$z, res$y)
  expect_equal(res$z, list(1))
})

test_that("summarise() unpacks unnamed tibble results (#2326)", {
  expect_equal(
    iris %>% group_by(Species) %>% summarise(
      tibble(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
    ),
    iris %>% group_by(Species) %>% summarise(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
  )
})

test_that("summarise() packs named tibble results (#2326)", {
  res <- iris %>%
    group_by(Species) %>%
    summarise(
      out = tibble(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
    )
  expect_is(res$out, "data.frame")
  expect_equal(nrow(res$out), 3L)
})
