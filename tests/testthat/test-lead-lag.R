context("Lead and lag")

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_is(lead(x), "Date")
  expect_is(lag(x), "Date")

  expect_is(lead(y), "POSIXct")
  expect_is(lag(y), "POSIXct")
})

test_that("#925 is fixed", {
  data <- tibble(
    name = c("Rob", "Pete", "Rob", "John", "Rob", "Pete", "John", "Pete", "John", "Pete", "Rob", "Rob"),
    time = c(3, 2, 5, 3, 2, 3, 2, 4, 1, 1, 4, 1)
  )
  res <- data %>% group_by(name) %>% mutate(lag_time = lag(time))
  expect_equal(
    res$lag_time[res$name == "Rob"],
    c(NA, head(data$time[data$name == "Rob"], -1))
  )
  expect_equal(
    res$lag_time[res$name == "Pete"],
    c(NA, head(data$time[data$name == "Pete"], -1))
  )
  expect_equal(
    res$lag_time[res$name == "John"],
    c(NA, head(data$time[data$name == "John"], -1))
  )
})

test_that("#937 is fixed", {
  df <- tibble(
    name = rep(c("Al", "Jen"), 3),
    score = rep(c(100, 80, 60), 2)
  )

  res <- df %>% group_by(name) %>% mutate(next.score = lead(score))
  expect_equal(
    res$next.score[res$name == "Al"],
    c(tail(df$score[df$name == "Al"], -1), NA)
  )
  expect_equal(
    res$next.score[res$name == "Jen"],
    c(tail(df$score[df$name == "Jen"], -1), NA)
  )
})

test_that("input checks", {
  expect_error(
    lead(letters, -1),
    "`n` must be a nonnegative integer scalar, not a double vector of length 1",
    fixed = TRUE
  )
  expect_error(
    lead(letters, "1"),
    "`n` must be a nonnegative integer scalar, not a character vector of length 1",
    fixed = TRUE
  )

  expect_error(
    lag(letters, -1),
    "`n` must be a nonnegative integer scalar, not a double vector of length 1",
    fixed = TRUE
  )
  expect_error(
    lag(letters, "1"),
    "`n` must be a nonnegative integer scalar, not a character vector of length 1",
    fixed = TRUE
  )
})

test_that("lead() and lag() respect bit64::integer64 (#4558)", {
  data <- c(1, 2, 3)
  x <- bit64::as.integer64(data)
  expect_equal(lead(x), bit64::as.integer64(lead(data)))
  expect_equal(lag(x) , bit64::as.integer64(lag(data)))

  y <- 3:1
  expect_equal(lead(x, order_by = y), bit64::as.integer64(lead(data, order_by = y)))
  expect_equal(lag(x, order_by = y) , bit64::as.integer64(lag(data, order_by = y)))
})
