context("Sample")

# Basic behaviour -------------------------------------------------------------

test_that("sample preserves class", {
  expect_is(sample_n(mtcars, 1), "data.frame")
  expect_is(sample_n(tbl_df(mtcars), 1), "tbl_df")

  expect_is(sample_frac(mtcars, 1), "data.frame")
  expect_is(sample_frac(tbl_df(mtcars), 1), "tbl_df")
})

# Ungrouped  -------------------------------------------------------------------

df <- data.frame(
  x = 1:2,
  y = c(0, 1)
)

test_that("sample respects weight", {
  expect_error(sample_n(df, 2, weight = y), "too few positive probabilities")
  expect_equal(sample_n(df, 1, weight = y)$x, 2)

  expect_error(sample_frac(df, 1, weight = y), "too few positive probabilities")
  expect_equal(sample_frac(df, 0.5, weight = y)$x, 2)
})

test_that("sample gives informative error for unknown type", {
  expect_error(sample_n(list()), "Don't know how to sample")
})

# Grouped ----------------------------------------------------------------------

test_that("sampling grouped tbl samples each group", {
  sampled <- mtcars %>% group_by(cyl) %>% sample_n(2)
  expect_is(sampled, "grouped_df")
  expect_equal(groups(sampled), list(quote(cyl)))
  expect_equal(nrow(sampled), 6)
  expect_equal(sampled$cyl, rep(c(4, 6, 8), each = 2))
})

test_that("can't sample more values than obs (without replacement)", {
  by_cyl <- mtcars %>% group_by(cyl)
  expect_error(sample_n(by_cyl, 10), "Do you want replace = TRUE")
})

df2 <- data.frame(
  x = rep(1:2, 2),
  y = rep(c(0, 1), 2),
  g = rep(1:2, each = 2)
)


test_that("grouped sample respects weight", {
  grp <- df2 %>% group_by(g)

  expect_error(sample_n(grp, 2, weight = y), "too few positive probabilities")
  expect_equal(sample_n(grp, 1, weight = y)$x, c(2, 2))

  expect_error(sample_frac(grp, 1, weight = y), "too few positive probabilities")
  expect_equal(sample_frac(grp, 0.5, weight = y)$x, c(2, 2))
})
