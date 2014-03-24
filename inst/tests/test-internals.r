context("internals")

test_that("comparisons<REALSXP> works as expected (#275)", {
  res <- test_comparisons()
  expect_true( all(res) )  
})

