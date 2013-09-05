context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
tbls <- clone_tbls(df)

test_that("two selects equivalent to one", {
  exp <- strip(select(df, n:o))
  
  expect_equal(strip(select(select(tbls$df, l:s), n:o)), exp)
  expect_equal(strip(select(select(tbls$dt, l:s), n:o)), exp)
  expect_equal(strip(select(select(tbls$sqlite, l:s), n:o)), exp)
})
