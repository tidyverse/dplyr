context("utils")

test_that("check_pkg() gives correct error message", {
  expect_error(
    dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz"),
    "The `__foobarbaz__` package is required to foobar a baz")
})
