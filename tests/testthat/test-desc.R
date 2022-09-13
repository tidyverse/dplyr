test_that("errors cleanly on non-vectors", {
  expect_snapshot(desc(mean), error = TRUE)
})
