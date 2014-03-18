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
