context("Group sizes")

test_that("ungrouped data has 1 group, with group size = nrow()", {
  df <- tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

  expect_equal(n_groups(df), 1L)
  expect_equal(group_size(df), 30)
})

test_that("rowwise data has one group for each group", {
  rw <- rowwise(mtcars)
  expect_equal(n_groups(rw), 32)
  expect_equal(group_size(rw), rep(1, 32))
})

test_that("group_size correct for grouped data", {
  df <- tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5)) %>% group_by(x)
  expect_equal(n_groups(df), 3L)
  expect_equal(group_size(df), rep(10, 3))
})


# For following tests, add an extra level that's not present in data
test_that("n_groups drops zero-length groups", {
  df <- tibble(x = factor(1:3, levels = 1:4)) %>% group_by(x)
  expect_equal(n_groups(df), 3)
})

test_that("summarise drops zero-length groups", {
  df <- tibble(x = factor(rep(1:3, each = 10)))

  out <- df %>%
    group_by(x) %>%
    summarise(n = n())

  expect_equal(out$n, c(10L, 10L, 10L))
})
