context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)
source_df <- clone_sources(df)

test_that("filter results independent of data source (simple)", {
  sel <- c("d", "g", "a")
  expected <- strip(df[df$a > 6, , drop = FALSE])

  expect_equal(strip(filter(source_df$df, a > 6)), expected)
  expect_equal(strip(filter(source_df$dt, a > 6)), expected)
  expect_equal(strip(filter(source_df$sqlite, a > 6)), expected)
})
