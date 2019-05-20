context("Arrange")

df2 <- data.frame(
  a = rep(c(NA, 1, 2, 3), each = 4),
  b = rep(c(0L, NA, 1L, 2L), 4),
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep(c(T, NA, F, T), each = 4),
  id = 1:16,
  stringsAsFactors = FALSE
)

equal_df <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  isTRUE(all.equal(x, y))
}

test_that("local arrange sorts missing values to end", {
  na_last <- function(x) {
    n <- length(x)
    all(is.na(x[(n - 3):n]))
  }

  # Numeric
  expect_true(na_last(arrange(df2, a)$a))
  expect_true(na_last(arrange(df2, desc(a))$a))

  # Integer
  expect_true(na_last(arrange(df2, b)$b))
  expect_true(na_last(arrange(df2, desc(b))$b))

  # Character
  expect_true(na_last(arrange(df2, c)$c))
  expect_true(na_last(arrange(df2, desc(c))$c))

  # Logical
  expect_true(na_last(arrange(df2, d)$d))
  expect_true(na_last(arrange(df2, desc(d))$d))
})

test_that("two arranges equivalent to one", {
  df <- tribble(
    ~ x,  ~ y,
    2,      1,
    2,     -1,
    1,      1
  )

  df1 <- df %>% arrange(x, y)
  df2 <- df %>% arrange(y) %>% arrange(x)

  expect_equal(df1, df2)
})

test_that("arrange handles list columns (#282)", {
  df <- data.frame(a = 2:1)
  df$b <- list("foo", "bar")
  res <- arrange(df, a)
  expect_equal(res$b, list("bar", "foo"))
})

test_that("arrange handles the case where ... is missing (#338)", {
  expect_equivalent(arrange(mtcars), mtcars)
})

test_that("arrange handles 0-rows data frames", {
  d <- data.frame(a = numeric(0))
  expect_identical(d, arrange(d))
})

test_that("grouped arrange ignores group (#491 -> #1206)", {
  df <- data.frame(g = c(2, 1, 2, 1), x = c(4:1))

  out <- df %>% group_by(g) %>% arrange(x)
  expect_equal(out$x, 1:4)
})

test_that("arrange keeps the grouping structure (#605)", {
  dat <- tibble(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
  res <- dat %>% group_by(g) %>% arrange()
  expect_is(res, "grouped_df")
  expect_equal(res$x, dat$x)

  res <- dat %>% group_by(g) %>% arrange(x)
  expect_is(res, "grouped_df")
  expect_equal(res$x, 1:4)
  expect_equal(group_rows(res), list(c(2, 4), c(1, 3)))
})

test_that("arrange handles complex vectors", {
  d <- data.frame(x = 1:10, y = 10:1 + 2i)
  res <- arrange(d, y)
  expect_equal(res$y, rev(d$y))
  expect_equal(res$x, rev(d$x))

  res <- arrange(res, desc(y))
  expect_equal(res$y, d$y)
  expect_equal(res$x, d$x)

  d$y[c(3, 6)] <- NA
  res <- arrange(d, y)
  expect_true(all(is.na(res$y[9:10])))

  res <- arrange(d, desc(y))
  expect_true(all(is.na(res$y[9:10])))
})

test_that("arrange respects S4 classes #1105", {
  TestS4 <- suppressWarnings(setClass("TestS4", contains = "numeric"))
  setMethod('[', 'TestS4', function(x, i, ...){ TestS4(unclass(x)[i, ...])  })
  on.exit(removeClass("TestS4"))

  df <- data.frame(p = TestS4(c(1, 2, 3)), x = 1:3)
  res <- arrange(df, p)
  expect_is(res$p, "TestS4")
})

test_that("arrange works with empty data frame (#1142)", {
  df <- data.frame()
  res <- df %>% arrange()
  expect_equal(nrow(res), 0L)
  expect_equal(length(res), 0L)
})

test_that("arrange respects locale (#1280)", {
  df2 <- tibble(words = c("casa", "\u00e1rbol", "zona", "\u00f3rgano"))

  res <- df2 %>% arrange(words)
  expect_equal(res$words, sort(df2$words))

  res <- df2 %>% arrange(desc(words))
  expect_equal(res$words, sort(df2$words, decreasing = TRUE))
})

test_that("duplicated column name is explicit about which column (#996)", {
  df <- data.frame(x = 1:10, x = 1:10)
  names(df) <- c("x", "x")

  # Error message created by tibble
  expect_error(df %>% arrange())

  df <- data.frame(x = 1:10, x = 1:10, y = 1:10, y = 1:10)
  names(df) <- c("x", "x", "y", "y")

  # Error message created by tibble
  expect_error(df %>% arrange())
})

test_that("arrange fails gracefully on list columns (#1489)", {
  df <- expand.grid(group = 1:2, y = 1, x = 1) %>%
    group_by(group) %>%
    do(fit = lm(data = ., y ~ x))
  expect_error(
    arrange(df, fit),
    "Argument 1 is of unsupported type list",
    fixed = TRUE
  )
})

test_that("arrange supports raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_identical(arrange(df, a), df)
  expect_identical(arrange(df, b), df)
  expect_identical(arrange(df, desc(a)), df[3:1, ])
  expect_identical(arrange(df, desc(b)), df[3:1, ])
})

test_that("arrange fails gracefully on matrix input (#1870)", {
  df <- tibble(a = 1:3, b = 4:6)
  expect_error(
    arrange(df, is.na(df)),
    "Argument 1 is of unsupported type matrix",
    fixed = TRUE
  )
})

test_that("arrange fails gracefully on data.frame input (#3153)", {
  df <- tibble(x = 1:150, iri = rnorm(150))
  expect_error(arrange(df, iris), "Argument 1 is of unsupported type data.frame")
})

test_that("arrange.data.frame recognizes the .by_group argument (#3546)", {
  df <- data.frame(foo=1:2, bar=2)
  res <- df %>%
    arrange(foo, .by_group=TRUE)
  expect_identical(res, df)
})

test_that("desc(<not just a symbol>) works (#4099)", {
  df <- data.frame(x = rep(1, 5), y = c(0, 3, 1.5, -5, 4)) %>%
    mutate(diff = x - y, absdiff = abs(x - y))
  expect_identical(
    arrange(df, desc(abs(diff))),
    arrange(df, desc(absdiff))
  )
})

test_that("arrange supports bit64::integer64 (#4366)", {
  df <- tibble(x = bit64::as.integer64(c(1, 3, 2, 1)))
  expect_identical(
    arrange(df, x),
    tibble(x = bit64::as.integer64(c(1, 1, 2, 3)))
  )
  expect_identical(
    arrange(df, desc(x)),
    tibble(x = bit64::as.integer64(c(3, 2, 1, 1)))
  )
  expect_identical(
    arrange(df, -x),
    tibble(x = bit64::as.integer64(c(3, 2, 1, 1)))
  )
})

# grouped_df --------------------------------------------------------------

test_that("can choose to include grouping vars", {
  df <- tibble(g = c(1, 2), x = c(2, 1)) %>% group_by(g)

  df1 <- df %>% arrange(x, .by_group = TRUE)
  df2 <- df %>% arrange(g, x)

  expect_equal(df1, df2)
})
