# ------------------------------------------------------------------------------
# `join_by()`

test_that("works with equi conditions", {
  by <- join_by(x == y, a == b)

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$condition, c("==", "=="))
  expect_identical(by$filter, c("none", "none"))
})

test_that("works with non-equi conditions", {
  by <- join_by(x == y, a > b, a >= b, a < b, a <= b)

  expect_identical(by$x, c("x", rep("a", 4)))
  expect_identical(by$y, c("y", rep("b", 4)))
  expect_identical(by$condition, c("==", ">", ">=", "<", "<="))
})

test_that("works with rolling conditions", {
  by <- join_by(x == y, max(a > b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "max"))

  by <- join_by(x == y, min(a > b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "min"))
})

test_that("works with single arguments", {
  by <- join_by(a, b)
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("a", "b"))
})

test_that("works with character strings", {
  by1 <- join_by("a", "b" == "c", max("d" >= "e"))
  by2 <- join_by(a, b  == c, max(d >= e))

  expect_identical(by1$condition, by2$condition)
  expect_identical(by1$filter, by2$filter)
  expect_identical(by1$x, by2$x)
  expect_identical(by1$y, by2$y)
})

test_that("can join_by() nothing for a cross join", {
  by <- join_by()
  expect_identical(by$x, character())
  expect_identical(by$y, character())
})

test_that("has an informative print method", {
  expect_snapshot(join_by())
  expect_snapshot(join_by(a, b))
  expect_snapshot(join_by("a", "b"))
  expect_snapshot(join_by(a == a, b >= c))
  expect_snapshot(join_by(a == a, b >= "c"))
  expect_snapshot(join_by(a == a, max(b >= "c"), min(d < e)))
})

test_that("has informative error messages", {
  # `=` rather than `==`
  expect_snapshot(error = TRUE, join_by(a = b))

  # Empty expression
  expect_snapshot(error = TRUE, join_by(NULL))

  # Improper rolling specification
  expect_snapshot(error = TRUE, join_by(foo(x > y)))

  # Length two expression inside rolling specification
  expect_snapshot(error = TRUE, join_by(max(!x)))

  # Improper separator
  expect_snapshot(error = TRUE, join_by(x == y, x ^ y))

  # Improper LHS
  expect_snapshot(error = TRUE, join_by(x + 1 == y))

  # Improper RHS
  expect_snapshot(error = TRUE, join_by(x == y + 1))
})

# ------------------------------------------------------------------------------
# `as_join_by()`

test_that("as_join_by() emits useful errors", {
  expect_snapshot(error = TRUE, as_join_by(FALSE))
})

# ------------------------------------------------------------------------------
# `join_by_common()`

test_that("automatically finds common variables", {
  x_names <- c("x", "y")
  y_names <- c("x", "z")
  expect_message(by <- join_by_common(x_names, y_names))
  expect_identical(by$x, "x")
  expect_identical(by$y, "x")
})

test_that("join_by_common() emits useful information", {
  # Common by message
  expect_snapshot(by <- join_by_common(c("x", "y"), c("x", "y")))

  # No common variables error
  expect_snapshot(error = TRUE, join_by_common(c("x", "y"), c("w", "z")))
})
