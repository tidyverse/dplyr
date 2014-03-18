context("Sample")

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


test_that("sample preserves class", {
  expect_is(sample_n(mtcars, 1), "data.frame")
  expect_is(sample_n(tbl_df(mtcars), 1), "tbl_df")
  expect_is(sample_n(data.table(mtcars), 1), "data.table")
  expect_is(sample_n(tbl_dt(mtcars), 1), "tbl_dt")

  expect_is(sample_frac(mtcars, 1), "data.frame")
  expect_is(sample_frac(tbl_df(mtcars), 1), "tbl_df")
  expect_is(sample_frac(data.table(mtcars), 1), "data.table")
  expect_is(sample_frac(tbl_dt(mtcars), 1), "tbl_dt")
})

test_that("sample gives informative error for unknown type", {
  expect_error(sample_n(list()), "Don't know how to sample")
})
