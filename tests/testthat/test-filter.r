context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)
sources <- clone_sources(df)

test_that("filter results independent of data source (simple)", {
  expected <- strip(df[df$a > 6, , drop = FALSE])

  expect_equal(strip(filter(sources$df, a > 6)), expected)
  expect_equal(strip(filter(sources$dt, a > 6)), expected)
  expect_equal(strip(filter(sources$sqlite, a > 6)), expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- strip(df[df$b %in% sel, , drop = FALSE])
  
  expect_equal(strip(filter(sources$df, b %in% sel)), expected)
  expect_equal(strip(filter(sources$dt, b %in% sel)), expected)
  expect_equal(strip(filter(sources$sqlite, b %in% sel)), expected)
})
