context("recode")

test_that("error if no arguments", {
  expect_error(recode(1:5), "No replacements provided")
  expect_error(recode("a"), "No replacements provided")
  expect_error(recode(factor("a")), "No replacements provided")
})

test_that("positional substitution works", {
  expect_equal(recode(1:2, "a", "b"), c("a", "b"))
})

test_that("names override positions", {
  expect_equal(recode(1:2, `2` = "b", `1` = "a"), c("a", "b"))
})

test_that("numeric vals must be all named or not named at all", {
  expect_error(
    recode(1:2, "b", `1` = "a"),
    "Either all values must be named, or none must be named"
  )
})

test_that("named substitution works", {
  x1 <- letters[1:3]
  x2 <- factor(x1)

  expect_equal(recode(x1, a = "apple"), c("apple", NA, NA))
  expect_equal(recode(x2, a = "apple"), factor(c("apple", NA, NA)))
})

test_that("missing values replaced by missing argument", {
  expect_equal(recode(c(1, NA), "a"), c("a", NA))
  expect_equal(recode(c(1, NA), "a", .missing = "b"), c("a", "b"))
})

test_that("unmatched value replaced by default argument", {
  expect_equal(recode(c(1, 2), "a"), c("a", NA))
  expect_equal(recode(c(1, 2), "a", .default = "b"), c("a", "b"))
})

test_that("missing and default place nicely together", {
  expect_equal(
    recode(c(1, 2, NA), "a", .default = "b", .missing = "c"),
    c("a", "b", "c")
  )
})

test_that("can give name x", {
  expect_equal(recode("x", x = "a"), "a")
})

test_that(".default works when not all values are named", {
  x <- rep(1:3, 3)
  expect_equal(recode(x, `3` = 10L, .default = x), rep(c(1L, 2L, 10L), 3))
})
