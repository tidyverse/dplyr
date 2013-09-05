context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)
tbls <- clone_tbls(df)

test_that("filter results independent of data tbl (simple)", {
  expected <- strip(df[df$a > 6, , drop = FALSE])

  expect_equal(strip(filter(tbls$df, a > 6)), expected)
  expect_equal(strip(filter(tbls$dt, a > 6)), expected)
  expect_equal(strip(filter(tbls$sqlite, a > 6)), expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- strip(df[df$b %in% sel, , drop = FALSE])

  expect_equal(strip(filter(tbls$df, b %in% sel)), expected)
  expect_equal(strip(filter(tbls$dt, b %in% sel)), expected)
  expect_equal(strip(filter(tbls$sqlite, b %in% sel)), expected)
})

test_that("two filters equivalent to one", {
  exp <- strip(filter(df, a > 4 & b == 4))
  
  expect_equal(strip(filter(filter(tbls$df, a > 4), b == 4)), exp)
  expect_equal(strip(filter(filter(tbls$dt, a > 4), b == 4)), exp)
  expect_equal(strip(filter(filter(tbls$sqlite, a > 4), b == 4)), exp)
})
