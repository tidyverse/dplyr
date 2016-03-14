context("recode")

test_that("error if no arguments", {
  expect_error(recode(1:5), "No replacements provided")
  expect_error(recode("a"), "No replacements provided")
  expect_error(recode(factor("a")), "No replacements provided")
})

test_that("positional substitution works", {
  expect_equal(recode(1:2, "a", "b"), c("a", "b"))
})

test_that("warning if names used with numeric input", {
  expect_warning(recode(1:2, a = "a"), "Names are ignored")
})

test_that("named substitution works", {
  x1 <- letters[1:3]
  x2 <- factor(x1)

  expect_equal(recode(x1, a = "apple"), c("apple", NA, NA))
  expect_equal(recode(x2, a = "apple"), factor(c("apple", NA, NA)))
})

test_that("missing values replaced by missing argument", {
  expect_equal(recode(c(1, NA), "a"), c("a", NA))
  expect_equal(recode(c(1, NA), "a", missing = "b"), c("a", "b"))
})

test_that("unmatched value replaced by default argument", {
  expect_equal(recode(c(1, 2), "a"), c("a", NA))
  expect_equal(recode(c(1, 2), "a", default = "b"), c("a", "b"))
})

test_that("missing and default place nicely together", {
  expect_equal(
    recode(c(1, 2, NA), "a", default = "b", missing = "c"),
    c("a", "b", "c")
  )
})
