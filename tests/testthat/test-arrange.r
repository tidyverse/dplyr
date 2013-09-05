context("Arrange")

df <- expand.grid(
  a = sample(letters, 5),
  b = sample(letters, 5),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)
tbls <- clone_tbls(df)

test_that("two arranges equivalent to one", {
  exp <- strip(arrange(df, a, b))
  
  expect_equal(strip(arrange(arrange(tbls$df, b), a)), exp)
  expect_equal(strip(arrange(arrange(tbls$dt, b), a)), exp)
  expect_equal(strip(arrange(arrange(tbls$sqlite, b), a)), exp)
})
