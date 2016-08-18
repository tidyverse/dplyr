context("add_tally")

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
  expect_equal(as.character(groups(res)), "val")

  res <- df %>% group_by(g, val) %>% add_tally()
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_equal(as.character(groups(res)), c("g", "val"))
})

test_that("add_tally can be given a weighting variable", {
  df <- data.frame(a = c(1, 1, 2, 2, 2), w = c(1, 1, 2, 3, 4))

  out <- df %>% group_by(a) %>% add_tally(wt = w)
  expect_equal(out$n, c(2, 2, 9, 9, 9))

  out <- df %>% group_by(a) %>% add_tally(wt = w + 1)
  expect_equal(out$n, c(4, 4, 12, 12, 12))
})
