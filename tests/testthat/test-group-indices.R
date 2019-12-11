context("Group indices")

test_that("group_indices from ungrouped or grouped gives same result", {
  res1 <- group_indices(mtcars, cyl, vs, am)
  res2 <- mtcars %>% group_by(cyl, vs, am) %>% group_indices()
  expect_equal(res1, res2)
})

test_that("group_indices handles the case where no variable is given (#867)", {
  res <- group_indices(mtcars)
  expect_true(all(res == 1L))
})

test_that("group_indices handles grouped data and no arguments", {
  res1 <- mtcars %>% group_by(cyl) %>% group_indices()
  res2 <- mtcars %>% group_indices(cyl)
  expect_equal(res1, res2)
})

test_that("group_indices can be used in mutate (#2160)", {
  res1 <- mtcars %>% mutate(., group_idx = group_indices(., cyl))
  res2 <- mtcars %>% mutate(group_idx = as.integer(factor(cyl)))
  expect_equal(res1, res2)
})

test_that("group indices are updated correctly for joined grouped data frames (#2330)", {
  d1 <- data.frame(x = 1:2, y = 1:2) %>% group_by(x, y)
  expect_equal(group_indices(d1), d1$x)

  d2 <- expand.grid(x = 1:2, y = 1:2)
  res <- inner_join(d1, d2, by = "x")
  expect_equal(group_indices(res), res$x)
})

test_that("group_indices() works for rowwise data (#3491)", {
  df <- rowwise(data.frame(x = 1:10))
  expect_equal(group_indices(df), 1:10)
})

test_that("group_indices() warns when passed extra arguments on grouped or rowwise data", {
  df <- rowwise(data.frame(x = 1:10))
  expect_warning(idx <- group_indices(df, x))
  expect_equal(idx, 1:10)

  expect_warning(idx <- group_indices(group_by(df,x), x))
  expect_equal(idx, 1:10)
})

test_that("group_indices() can be used inside mutate (#1185)", {
  df <- tibble(v1 = c(3, 3, 2, 2, 3, 1), v2 = 1:6) %>% group_by(v1)
  expect_identical(
    pull(mutate(df, g = group_indices())),
    group_indices(df)
  )

  df <- tibble(v1 = c(3, 3, 2, 2, 3, 1), v2 = 1:6)
  expect_identical(
    pull(mutate(df, g = group_indices())),
    group_indices(df)
  )

  df <- rowwise(tibble(v1 = c(3, 3, 2, 2, 3, 1), v2 = 1:6))
  expect_identical(
    pull(mutate(df, g = group_indices())),
    group_indices(df)
  )
})

test_that("group_indices() recognizes .drop", {
  d <- tibble(f = factor("b", levels = c("a", "b", "c")))
  expect_equal(group_indices(d, f), 1L)
  expect_equal(group_indices(d, f, .drop = FALSE), 2L)

  # these two should return the same result (#4208):
  #   d %>% group_indices(...)
  #   d %>% group_by(...) %>% group_indices()
  d2 <- group_by(d, f)
  expect_equal(group_indices(d2), 1L)
})
