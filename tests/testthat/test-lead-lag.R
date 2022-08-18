# ------------------------------------------------------------------------------
# lead() / lag()

test_that("`lead()` / `lag()` get the direction right", {
  expect_identical(lead(1:5), c(2:5, NA))
  expect_identical(lag(1:5), c(NA, 1:4))
})

test_that("`lead()` / `lag()` catch negative `n`", {
  expect_snapshot(error = TRUE, {
    lead(1:5, -1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, -1)
  })
})

test_that("`lead()` / `lag()` check `n` properties before checking if positive", {
  # To prove that the `check_shift_n()` in `lag()` and `lead()` is required

  expect_snapshot(error = TRUE, {
    lead(1:5, n = 1:2)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = 1:2)
  })

  expect_snapshot(error = TRUE, {
    lead(1:5, n = "x")
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = "x")
  })

  expect_snapshot(error = TRUE, {
    lead(1:5, n = NA_integer_)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = NA_integer_)
  })
})

test_that("`lead()` / `lag()` check for empty dots", {
  expect_snapshot(error = TRUE, {
    lead(1:5, deault = 1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, deault = 1)
  })
})

test_that("`lag()` errors on <ts> objects", {
  expect_snapshot(error = TRUE, {
    lag(ts(1:10))
  })
})

test_that("lead() and lag() work for matrices (#5028)", {
  m <- matrix(1:6, ncol = 2)
  expect_equal(lag(m, 1), matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol = 2))
  expect_equal(lag(m, 1, default = NA), matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol= 2))

  expect_equal(lead(m, 1), matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2))
  expect_equal(lead(m, 1, default = NA), matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2))
})

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_s3_class(lead(x), "Date")
  expect_s3_class(lag(x), "Date")

  expect_s3_class(lead(y), "POSIXct")
  expect_s3_class(lag(y), "POSIXct")
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

test_that("`lead()` / `lag()` require that `x` is a vector", {
  expect_snapshot(error = TRUE, {
    lead(environment())
  })
  expect_snapshot(error = TRUE, {
    lag(environment())
  })
})

# ------------------------------------------------------------------------------
# shift()

test_that("works with all 4 combinations of with/without `default` and lag/lead", {
  x <- 1:5

  expect_identical(shift(x, n = 2L), c(NA, NA, 1L, 2L, 3L))
  expect_identical(shift(x, n = 2L, default = 0L), c(0L, 0L, 1L, 2L, 3L))

  expect_identical(shift(x, n = -2L), c(3L, 4L, 5L, NA, NA))
  expect_identical(shift(x, n = -2L, default = 0L), c(3L, 4L, 5L, 0L, 0L))
})

test_that("works with size 0 input", {
  x <- integer()

  expect_identical(shift(x, n = 2L), x)
  expect_identical(shift(x, n = 2L, default = 3L), x)
  expect_identical(shift(x, n = -2L), x)
  expect_identical(shift(x, n = -2L, default = 3L), x)
})

test_that("works with `n = 0` with and without `default`", {
  x <- 1:5

  expect_identical(shift(x, n = 0L), x)
  expect_identical(shift(x, n = 0L, default = -1L), x)

  x <- integer()

  expect_identical(shift(x, n = 0L), x)
  expect_identical(shift(x, n = 0L, default = -1L), x)
})

test_that("works with data frames", {
  df <- tibble(a = 1:3, b = letters[1:3])

  expect_identical(shift(df, n = 1), vec_slice(df, c(NA, 1, 2)))
  expect_identical(shift(df, n = -1), vec_slice(df, c(2, 3, NA)))

  default <- tibble(a = 0L, b = "")

  expect_identical(
    shift(df, n = 2, default = default),
    vec_c(default, default, vec_slice(df, 1))
  )
})

test_that("is affected by `order_by`", {
  x <- 1:5
  order_by <- c(2, 3, 2, 1, 5)

  expect_identical(
    shift(x, n = 1, order_by = order_by),
    c(4L, 3L, 1L, NA, 2L)
  )
  expect_identical(
    shift(x, n = -2, order_by = order_by),
    c(2L, NA, 5L, 3L, NA)
  )
})

test_that("`default` is cast to the type of `x` (#6330)", {
  expect_identical(shift(1L, default = 2), 2L)

  expect_snapshot(error = TRUE, {
    shift(1L, default = 1.5)
  })
})

test_that("`default` must be size 1 (#5641)", {
  expect_snapshot(error = TRUE, {
    shift(1:5, default = 1:2)
  })
  expect_snapshot(error = TRUE, {
    shift(1:5, default = integer())
  })
})

test_that("`n` is validated", {
  expect_snapshot(error = TRUE, {
    shift(1, n = 1:2)
  })
  expect_snapshot(error = TRUE, {
    shift(1, n = "x")
  })
  expect_snapshot(error = TRUE, {
    shift(1, n = NA_integer_)
  })
})

test_that("`order_by` must be the same size as `x`", {
  expect_snapshot(error = TRUE, {
    shift(1:5, order_by = 1:4)
  })
})



test_that("lead and lag simple hybrid version gives correct results (#133)", {
  res <- group_by(mtcars, cyl) %>%
    mutate(disp_lag_2 = lag(disp, 2), disp_lead_2 = lead(disp, 2)) %>%
    summarise(
      lag1 = all(is.na(head(disp_lag_2, 2))),
      lag2 = all(!is.na(tail(disp_lag_2, -2))),

      lead1 = all(is.na(tail(disp_lead_2, 2))),
      lead2 = all(!is.na(head(disp_lead_2, -2)))
    )

  expect_true(all(res$lag1))
  expect_true(all(res$lag2))

  expect_true(all(res$lead1))
  expect_true(all(res$lead2))
})


test_that("lag and lead work on factors inside mutate (#955)", {
  test_factor <- factor(rep(c("A", "B", "C"), each = 3))
  exp_lag  <- test_factor != lag(test_factor)
  exp_lead <- test_factor != lead(test_factor)

  test_df <- tibble(test = test_factor)
  res <- test_df %>% mutate(
    is_diff_lag  = (test != lag(test)),
    is_diff_lead = (test != lead(test))
  )
  expect_equal(exp_lag, res$is_diff_lag)
  expect_equal(exp_lead, res$is_diff_lead)
})

test_that("lag handles default argument in mutate (#915)", {
  blah <- data.frame(x1 = c(5, 10, 20, 27, 35, 58, 5, 6), y = 8:1)
  blah <- mutate(blah,
    x2 = x1 - lag(x1, n = 1, default = 0),
    x3 = x1 - lead(x1, n = 1, default = 0),
    x4 = lag(x1, n = 1L, order_by = y),
    x5 = lead(x1, n = 1L, order_by = y)
  )
  expect_equal(blah$x2, blah$x1 - lag(blah$x1, n = 1, default = 0))
  expect_equal(blah$x3, blah$x1 - lead(blah$x1, n = 1, default = 0))
  expect_equal(blah$x4, lag(blah$x1, n = 1L, order_by = blah$y))
  expect_equal(blah$x5, lead(blah$x1, n = 1L, order_by = blah$y))
})

test_that("If n = 0, lead and lag return x", {
  expect_equal(lead(1:2, 0), 1:2)
  expect_equal(lag(1:2, 0), 1:2)
})

test_that("If n = length(x), returns all missing", {
  miss <- rep(NA_integer_, 2)

  expect_equal(lead(1:2, 2), miss)
  expect_equal(lag(1:2, 2), miss)
})
