context("recode")

test_that("positional substitution works", {
  expect_equal(recode(1:2, "a", "b"), c("a", "b"))
})

test_that("names override positions", {
  expect_equal(recode(1:2, `2` = "b", `1` = "a"), c("a", "b"))
})

test_that("named substitution works", {
  x1 <- letters[1:3]
  x2 <- factor(x1)

  expect_equal(recode(x1, a = "apple", .default = NA_character_), c("apple", NA, NA))
  expect_equal(recode(x2, a = "apple", .default = NA_character_), factor(c("apple", NA, NA)))
})

test_that("missing values replaced by missing argument", {
  expect_equal(recode(c(1, NA), "a"), c("a", NA))
  expect_equal(recode(c(1, NA), "a", .missing = "b"), c("a", "b"))
  expect_equal(recode(c(letters[1:3], NA), .missing = "A"), c("a", "b", "c", "A"))
})

test_that("unmatched value replaced by default argument", {
  expect_warning(expect_equal(recode(c(1, 2), "a"), c("a", NA)))
  expect_equal(recode(c(1, 2), "a", .default = "b"), c("a", "b"))
  expect_equal(recode(letters[1:3], .default = "A"), c("A", "A", "A"))
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

test_that(".default is aliased to .x when missing and compatible", {
  x <- letters[1:3]
  expect_equal(recode(x, a = "A"), c("A", "b", "c"))

  n <- 1:3
  expect_equal(recode(n, `1` = 10L), c(10L, 2L, 3L))
})

test_that(".default is not aliased to .x when missing and not compatible", {
  x <- letters[1:3]
  expect_warning(expect_equal(recode(x, a = 1), c(1L, NA, NA)))

  n <- 1:3
  expect_warning(expect_equal(recode(n, `1` = "a"), c("a", NA, NA)))
})

test_that("conversion of unreplaced values to NA gives warning", {
  expect_warning(recode(1:3, `1` = "a"), "treated as NA")
  expect_warning(recode_factor(letters[1:3], b = 1, c = 2))
})

test_that(".dot argument works correctly (PR #2110)", {
  x1 <- letters[1:3]
  x2 <- 1:3
  x3 <- factor(x1)

  expect_equal(
    recode(x1, a = "apple", b = "banana", .default = NA_character_),
    recode(x1, .default = NA_character_, !!!list(a = "apple", b = "banana"))
  )
  expect_equal(
    recode(x1, a = "apple", b = "banana", .default = NA_character_),
    recode(x1, a = "apple", .default = NA_character_, !!!list(b = "banana"))
  )
  expect_equal(
    recode(x2, "1" = 4, "2" = 5, .default = NA_real_),
    recode(x2, .default = NA_real_, !!!list("1" = 4, "2" = 5))
  )
  expect_equal(
    recode(x2, "1" = 4, "2" = 5, .default = NA_real_),
    recode(x2, "1" = 4, .default = NA_real_, !!!list("2" = 5))
  )
  expect_equal(
    recode_factor(x3, a = "apple", b = "banana", .default = NA_character_),
    recode_factor(x3, .default = NA_character_, !!!list(a = "apple", b = "banana"))
  )
})


# factor ------------------------------------------------------------------

test_that("default .default works with factors", {
  expect_equal(recode(factor(letters[1:3]), a = "A"), factor(c("A", "b", "c")))
})

test_that("can recode factor to double", {
  f <- factor(letters[1:3])

  expect_equal(recode(f, a = 1, b = 2, c = 3), c(1, 2, 3))
  expect_equal(recode(f, a = 1, b = 2), c(1, 2, NA))
  expect_equal(recode(f, a = 1, b = 2, .default = 99), c(1, 2, 99))
})

test_that("recode_factor() handles .missing and .default levels", {
  x <- c(1:3, NA)
  expect_warning(
    expect_equal(
      recode_factor(x, `1` = "z", `2` = "y"),
      factor(c("z", "y", NA, NA), levels = c("z", "y"))
    )
  )
  expect_equal(
    recode_factor(x, `1` = "z", `2` = "y", .default = "D"),
    factor(c("z", "y", "D", NA), levels = c("z", "y", "D"))
  )
  expect_equal(
    recode_factor(x, `1` = "z", `2` = "y", .default = "D", .missing = "M"),
    factor(c("z", "y", "D", "M"), c("z", "y", "D", "M"))
  )
})

test_that("recode_factor() handles vector .default", {
  expected <- factor(c("a", "z", "y"), levels = c("z", "y", "a"))
  x1 <- letters[1:3]
  x2 <- factor(x1)

  expect_equal(recode_factor(x1, b = "z", c = "y"), expected)
  expect_equal(recode_factor(x2, b = "z", c = "y"), expected)

  expect_equal(recode_factor(x1, b = "z", c = "y", .default = x1), expected)
  expect_equal(recode_factor(x2, b = "z", c = "y", .default = x1), expected)
})

test_that("can recode factor with redundant levels", {
  expect_equal(
    recode(factor(letters[1:4]), d = "c", b = "a"),
    factor(c("a", "a", "c", "c"), levels = c("a", "c"))
  )
  expect_equal(
    recode_factor(letters[1:4], d = "c", b = "a"),
    factor(c("a", "a", "c", "c"), levels = c("c", "a"))
  )
})

# Errors --------------------------------------------

test_that("recode() gives meaningful error messages", {
  verify_output(test_path("test-recode-errors.txt"), {
    recode(factor("a"), a = 5, .missing = 10)

    recode("a", b = 5, "c")
    recode(factor("a"), b = 5, "c")

    "# no replacement"
    recode(1:5)
    recode("a")
    recode(factor("a"))
  })
})
