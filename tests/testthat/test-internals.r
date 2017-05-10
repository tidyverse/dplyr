context("internals")

test_that("comparisons<REALSXP> works as expected (#275)", {
  res <- test_comparisons()
  expect_true(all(res))
})

test_that("join_match() works as expected", {
  res <- test_matches()
  expect_true(all(unlist(res)))
})

test_that("wrapping of length values works as expected", {
  res <- test_length_wrap()
  expect_true(all(res))
})
