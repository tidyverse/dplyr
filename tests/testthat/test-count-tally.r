context("count-tally")

# count -------------------------------------------------------------------

test_that("must manually supply name when n column already present", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))
  expect_error(count(df, n), "already present")
  expect_named(count(df, n, name = "nn"), c("n", "nn"))

  df <- data.frame(g = c(1, 2, 2, 2), n = c(1:4))
  expect_error(count(df, g), "already present")
  expect_named(count(df, g, name = "n"), c("g", "n"))
})

test_that("count preserves grouping of input", {
  df <- data.frame(g = c(1, 2, 2, 2))

  out1 <- count(df, g)
  expect_equal(group_vars(out1), character())

  df2 <- df %>% group_by(g)
  out2 <- count(df2)
  expect_equal(group_vars(out2), "g")
})

test_that("grouped count includes group", {
  df <- data.frame(g = c(1, 2, 2, 2))

  res <- df %>% group_by(g) %>% count()
  expect_equal(names(res), c("g", "n"))
  expect_equal(res$n, c(1, 3))
  expect_equal(group_vars(res), "g")
})

test_that("can override variable name", {
  df <- data.frame(g = c(1, 1, 2, 2, 2))
  expect_named(count(df, g, name = "xxx"), c("g", "xxx"))
})

test_that("count() does not ignore non-factor empty groups (#4013)",  {
  d <- data.frame(x = c("a", "a", "b", "b"),
    value = 1:4,
    stringsAsFactors = FALSE)

  g <- d %>%
    group_by(x) %>%
    filter(value > 3, .preserve = TRUE)

  res <- count(g)
  expect_equal(nrow(res), 2L)
  expect_equal(res$x, c("a", "b"))
  expect_equal(res$n, c(0L, 1L))
})

test_that("count() deals with .drop", {
  d <- tibble(
    f1 = factor("b", levels = c("a", "b", "c")),
    f2 = factor("g", levels = c("e", "f", "g")),
    x  = 48
  )
  res <- d %>%
    group_by(f1, .drop = TRUE) %>%
    count(f2, .drop = TRUE)

  res2 <- d %>%
    group_by(f1, .drop = TRUE) %>%
    count(f2)

  res3 <- d %>%
    group_by(f1, .drop = TRUE) %>%
    count(f2, .drop = FALSE)

  expect_equal(n_groups(res), 1L)
  expect_identical(res, res2)
  expect_equal(n_groups(res3), 3L)
  expect_equal(nrow(res3), 9L)
})

test_that("add_count() respects .drop",  {
  d <- tibble(
    f1 = factor("b", levels = c("a", "b", "c")),
    f2 = factor("g", levels = c("e", "f", "g")),
    x  = 48
  )
  res1 <- d %>% group_by(f1) %>% add_count(f2, .drop = FALSE)
  res2 <- d %>% group_by(f1) %>% add_count(f2, .drop = TRUE)
  res3 <- d %>% group_by(f1) %>% add_count(f2)

  expect_identical(res2, res3)
  expect_equal(n_groups(res2), 1)
  expect_equal(n_groups(res1), 3)
})

# add_count ---------------------------------------------------------------

test_that("must manually supply name when n column already present", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))
  expect_error(add_count(df, n), "already present")
  expect_named(add_count(df, n, name = "nn"), c("n", "nn"))
})

test_that("respects and preserves existing groups", {
  df <- data.frame(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  res <- df %>% add_count(val)
  expect_equal(res$n, c(3, 3, 3, 1))
  expect_no_groups(res)

  res <- df %>% group_by(g) %>% add_count(val)
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_groups(res, "g")
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(add_count(df, g, name = "xxx"), c("g", "x", "xxx"))
})

# tally -------------------------------------------------------------------

test_that("weighted tally drops NAs (#1145)", {
  df <- tibble(x = c(1, 1, NA))
  expect_equal(tally(df, x)$n, 2)
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(tally(df, name = "xxx"), "xxx")
})

test_that("tally uses variable named n as default wt.", {
  df <- tibble(n = 1:3)
  expect_message(res <- df %>% tally(name = "nn"), "Using `n` as weighting variable")
  expect_named(res, "nn")
})

# add_tally ---------------------------------------------------------------

test_that("can add tallies of a variable", {
  df <- data.frame(a = c(1, 1, 2, 2, 2))

  out <- df %>% group_by(a) %>% add_tally()
  expect_equal(names(out), c("a", "n"))
  expect_equal(out$a, df$a)
  expect_equal(out$n, c(2, 2, 3, 3, 3))

  out <- df %>% group_by(a) %>% add_tally(sort = TRUE)
  expect_equal(out$n, c(3, 3, 3, 2, 2))
})

test_that("add_tally respects and preserves existing groups", {
  df <- data.frame(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  res <- df %>% group_by(val) %>% add_tally()
  expect_equal(res$n, c(3, 3, 3, 1))
  expect_groups(res, "val")

  res <- df %>% group_by(g, val) %>% add_tally()
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_groups(res, c("g", "val"))
})

test_that("add_tally can be given a weighting variable", {
  df <- data.frame(a = c(1, 1, 2, 2, 2), w = c(1, 1, 2, 3, 4))

  out <- df %>% group_by(a) %>% add_tally(wt = w)
  expect_equal(out$n, c(2, 2, 9, 9, 9))

  out <- df %>% group_by(a) %>% add_tally(wt = w + 1)
  expect_equal(out$n, c(4, 4, 12, 12, 12))
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(add_tally(df, name = "xxx"), c("g", "x", "xxx"))
})
