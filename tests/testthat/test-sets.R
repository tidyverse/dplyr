context("Set ops")

test_that("set operation give useful error message. #903", {
  alfa <- tibble(
    land = c("Sverige", "Norway", "Danmark", "Island", "GB"),
    data = rnorm(length(land))
  )

  beta <- tibble(
    land = c("Norge", "Danmark", "Island", "Storbritannien"),
    data2 = rnorm(length(land))
  )
  expect_error(
    intersect(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`.\n- Cols in x but not y: `data`.",
    fixed = TRUE
  )
  expect_error(
    union(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`.\n- Cols in x but not y: `data`.",
    fixed = TRUE
  )
  expect_error(
    setdiff(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`.\n- Cols in x but not y: `data`.",
    fixed = TRUE
  )
})

test_that("set operations use coercion rules (#799)", {
  df1 <- tibble(x = 1:2, y = c(1, 1))
  df2 <- tibble(x = 1:2, y = 1:2)

  expect_equal(nrow(union(df1, df2)), 3L)
  expect_equal(nrow(intersect(df1, df2)), 1L)
  expect_equal(nrow(setdiff(df1, df2)), 1L)

  df1 <- tibble(x = factor(letters[1:10]))
  df2 <- tibble(x = letters[6:15])
  res <- intersect(df1, df2)
  expect_equivalent(res, tibble(x = letters[6:10]))

  res <- intersect(df2, df1)
  expect_equivalent(res, tibble(x = letters[6:10]))

  res <- union(df1, df2)
  expect_equivalent(res, tibble(x = letters[1:15]))
  res <- union(df2, df1)
  expect_equivalent(res, tibble(x = letters[c(6:15, 1:5)]))

  res <- setdiff(df1, df2)
  expect_equivalent(res, tibble(x = letters[1:5]))
  res <- setdiff(df2, df1)
  expect_equivalent(res, tibble(x = letters[11:15]))
})

test_that("setdiff handles factors with NA (#1526)", {
  df1 <- tibble(x = factor(c(NA, "a")))
  df2 <- tibble(x = factor("a"))

  res <- setdiff(df1, df2)
  expect_is(res$x, "factor")
  expect_equal(levels(res$x), "a")
  expect_true(is.na(res$x[1]))
})

test_that("intersect does not unnecessarily coerce (#1722)", {
  df <- tibble(a = 1L)
  res <- intersect(df, df)
  expect_is(res$a, "integer")
})

test_that("set operations reconstruct grouping metadata (#3587)", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% group_by(g)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equal(setdiff(df1, df2), filter(df1, x < 3))
  expect_equal(intersect(df1, df2), filter(df1, x >= 3))
  expect_equal(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)) %>% group_by(g))

  expect_equal(setdiff(df1, df2) %>% group_rows(), list_of(1:2))
  expect_equal(intersect(df1, df2) %>% group_rows(), list_of(1:2))
  expect_equal(union(df1, df2) %>% group_rows(), list_of(1:2, 3:4, 5:6))
})

test_that("set operations keep the ordering of the data (#3839)", {
  rev_df <- function(df) {
    df[rev(seq_len(nrow(df))), ]
  }

  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2))
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equivalent(setdiff(df1, df2), filter(df1, x < 3))
  expect_equivalent(setdiff(rev_df(df1), df2), filter(rev_df(df1), x < 3))
  expect_equivalent(intersect(df1, df2), filter(df1, x >= 3))
  expect_equivalent(intersect(rev_df(df1), df2), filter(rev_df(df1), x >= 3))
  expect_equivalent(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)))
  expect_equivalent(union(rev_df(df1), df2), tibble(x = c(4:1, 5:6), g = rep(c(2:1, 3L), each = 2)))
  expect_equivalent(union(df1, rev_df(df2)), tibble(x = c(1:4, 6:5), g = rep(1:3, each = 2)))
})

test_that("set operations remove duplicates", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% bind_rows(., .)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equivalent(setdiff(df1, df2), filter(df1, x < 3) %>% distinct())
  expect_equivalent(intersect(df1, df2), filter(df1, x >= 3) %>% distinct())
  expect_equivalent(union(df1, df2), tibble(x = 1:6, g = rep(1:3, each = 2)))
})

test_that("set equality", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% group_by(g)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_true(setequal(df1, df1))
  expect_true(setequal(df2, df2))
  expect_false(setequal(df1, df2))
  expect_false(setequal(df2, df1))
})
