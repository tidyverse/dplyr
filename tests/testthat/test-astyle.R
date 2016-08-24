context("astyle")

test_that("source code formatting", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_travis()

  expect_warning(astyle("--dry-run"), NA)
})
