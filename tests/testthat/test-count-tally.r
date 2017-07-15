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


# tally -------------------------------------------------------------------

test_that("weighted tally drops NAs (#1145)", {
  df <- data_frame(x = c(1, 1, NA))

  expect_equal(tally(df, x)$n, 2)
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

