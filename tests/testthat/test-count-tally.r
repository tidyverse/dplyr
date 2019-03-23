context("count-tally")

# count -------------------------------------------------------------------

test_that("can count variable called n", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))

  out <- df %>% count(n)
  expect_equal(names(out), c("n", "nn"))
  expect_equal(out$nn, c(2, 3))

  out <- df %>% count(n, sort = TRUE)
  expect_equal(out$nn, c(3, 2))
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

test_that("returns user defined variable name", {
  df <- data.frame(g = c(1, 1, 2, 2, 2))
  var_name <- "number_n"
  res <- df %>% count(g, name = var_name)

  expect_equal(names(res), c("g", var_name))
  expect_equal(res[[var_name]], c(2, 3))
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

test_that("returns error if user-defined name equals a grouped variable name", {
  df <- data.frame(g = c(1, 1, 2, 2, 2))

  expect_error(df %>% count(g, name = "g"))
})

# add_count ---------------------------------------------------------------

test_that("can add counts of a variable called n", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))

  out <- df %>% add_count(n)
  expect_equal(names(out), c("n", "nn"))
  expect_equal(out$n, df$n)
  expect_equal(out$nn, c(2, 2, 3, 3, 3))

  out <- df %>% add_count(n, sort = TRUE)
  expect_equal(out$nn, c(3, 3, 3, 2, 2))
})

test_that("add_count respects and preserves existing groups", {
  df <- data.frame(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  res <- df %>% add_count(val)
  expect_equal(res$n, c(3, 3, 3, 1))
  expect_no_groups(res)

  res <- df %>% group_by(g) %>% add_count(val)
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_groups(res, "g")
})

test_that("adds counts column with user-defined name if it is not a grouped variable", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), count = c(3, 2, 5, 5, 5))
  name <- "count"

  out <- df %>% add_count(g, name = name)
  expect_equal(names(out), c("g", name))
  expect_equal(out[[name]], c(2, 2, 3, 3, 3))
})

test_that("returns error if user-defined name equals a grouped variable", {
  df <- data.frame(g = c(1, 1, 2, 2, 2))

  expect_error(df %>% add_count(g, name = "g"))
})

# tally -------------------------------------------------------------------

test_that("weighted tally drops NAs (#1145)", {
  df <- tibble(x = c(1, 1, NA))

  expect_equal(tally(df, x)$n, 2)
})

test_that("returns column with user-defined name", {
  df <- tibble(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  name <- "counts"
  res <- df %>% tally(name = name)

  expect_equal(names(res), name)
})

test_that("returns column with user-defined name if it is not a grouped variable", {
  df <- tibble(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  name <- "g"
  res <- df %>% tally(name = name)

  expect_equal(names(res), name)
})

test_that("returns error if user-defined name equals a grouped variable", {
  df <- tibble(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  name <- "g"

  expect_error(df %>% group_by(g) %>% tally(name = name))
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

test_that("adds column with user-defined variable name if it is not a grouped variable name", {
  df <- tibble(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  name <- "val"
  res <- df %>% add_tally(name = name)

  expect_equal(names(res), c("g", "val"))
})

test_that("returns error if user-defined name equals a grouped variable", {
  df <- tibble(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  name <- "val"
  expect_error(df %>% group_by(val) %>% add_tally(name = name))
})

# count and .drop ----------------------------------------------------

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

