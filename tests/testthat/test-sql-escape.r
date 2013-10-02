context("SQL: escaping")

# Identifiers ------------------------------------------------------------------

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

# Missing values ----------------------------------------------------------------

test_that("missing vaues become null", {
  expect_equal(escape(NA), sql("NULL"))
  expect_equal(escape(NA_real_), sql("NULL"))
  expect_equal(escape(NA_integer_), sql("NULL"))  
  expect_equal(escape(NA_character_), sql("NULL"))
})
