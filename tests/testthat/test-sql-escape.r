context("SQL: escaping")


ei <- function(...) unclass(escape(ident(...)))

test_that("identifiers are doubled quoted", {
  expect_equal(ei("x"), '"x"')
})

test_that("identifiers are comma separated", {
  expect_equal(ei("x", "y"), '"x", "y"')
})

test_that("identifier names become AS", {
  expect_equal(ei(x = "y"), '"y" AS "x"')
})
