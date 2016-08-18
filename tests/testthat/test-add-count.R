context("add_count")

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
  expect_null(groups(res))

  res <- df %>% group_by(g) %>% add_count(val)
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_equal(as.character(groups(res)), "g")
})
