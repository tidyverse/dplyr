context("SQL: escaping")

# Identifiers ------------------------------------------------------------------

ei <- function(...) unclass(escape(ident(c(...))))

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


# Times -------------------------------------------------------------------

test_that("times are converted to ISO 8601", {
  x <- ISOdatetime(2000, 1, 2, 3, 4, 5, tz = "US/Central")
  expect_equal(escape(x), sql("'2000-01-02T09:04:05Z'"))
})

