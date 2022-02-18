# ------------------------------------------------------------------------------
# `join_by()`

test_that("works with equi conditions", {
  by <- join_by(x == y, a == b)

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$condition, c("==", "=="))
  expect_identical(by$filter, c("none", "none"))
  expect_identical(by$cross, FALSE)
})

test_that("works with non-equi conditions", {
  by <- join_by(x == y, a > b, a >= b, a < b, a <= b)

  expect_identical(by$x, c("x", rep("a", 4)))
  expect_identical(by$y, c("y", rep("b", 4)))
  expect_identical(by$condition, c("==", ">", ">=", "<", "<="))
})

test_that("works with rolling conditions", {
  by <- join_by(x == y, preceding(a, b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "max"))
  expect_identical(by$condition, c("==", ">="))

  by <- join_by(x == y, preceding(a, b, inclusive = FALSE))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "max"))
  expect_identical(by$condition, c("==", ">"))

  by <- join_by(x == y, following(a, b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "min"))
  expect_identical(by$condition, c("==", "<="))

  by <- join_by(x == y, following(a, b, inclusive = FALSE))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "min"))
  expect_identical(by$condition, c("==", "<"))
})

test_that("works with single arguments", {
  by <- join_by(a, b)
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("a", "b"))
})

test_that("works with character strings", {
  by1 <- join_by("a", "b" == "c", preceding("d", "e"))
  by2 <- join_by(a, b  == c, preceding(d, e))

  expect_identical(by1$condition, by2$condition)
  expect_identical(by1$filter, by2$filter)
  expect_identical(by1$x, by2$x)
  expect_identical(by1$y, by2$y)
})

test_that("works with explicit referencing", {
  by <- join_by(x$a == y$b)
  expect_identical(by$x, "a")
  expect_identical(by$y, "b")

  by <- join_by(y$a == x$b)
  expect_identical(by$x, "b")
  expect_identical(by$y, "a")
})

test_that("join condition is correctly reversed with explicit referencing", {
  by <- join_by(y$a == x$a, y$a >= x$a, y$a > x$a, y$a <= x$a, y$a < x$a)
  expect_identical(by$condition, c("==", "<=", "<", ">=", ">"))
})

test_that("between conditions expand correctly", {
  by <- join_by(between(a, b, c))
  expect_identical(by$x, c("a", "a"))
  expect_identical(by$y, c("b", "c"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(between(y$a, x$b, x$c))
  expect_identical(by$x, c("b", "c"))
  expect_identical(by$y, c("a", "a"))
  expect_identical(by$condition, c("<=", ">="))
})

test_that("overlaps / within conditions expand correctly", {
  by <- join_by(overlaps(a, b, c, d))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("d", "c"))
  expect_identical(by$condition, c("<=", ">="))

  by <- join_by(overlaps(y$a, y$b, x$b, x$c))
  expect_identical(by$x, c("c", "b"))
  expect_identical(by$y, c("a", "b"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(within(a, b, c, d))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("c", "d"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(within(y$a, y$b, x$b, x$c))
  expect_identical(by$x, c("b", "c"))
  expect_identical(by$y, c("a", "b"))
  expect_identical(by$condition, c("<=", ">="))
})

test_that("between / overlaps / within / preceding / following can use named arguments", {
  by <- join_by(between(a, y_upper = b, y_lower = c))
  expect_identical(by$x, c("a", "a"))
  expect_identical(by$y, c("c", "b"))

  by <- join_by(overlaps(y_lower = c, y_upper = d, x_lower = a, x_upper = b))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("d", "c"))
  expect_identical(by$condition, c("<=", ">="))

  by <- join_by(overlaps(y_lower = x$c, y_upper = x$d, x_lower = y$a, x_upper = y$b))
  expect_identical(by$x, c("d", "c"))
  expect_identical(by$y, c("a", "b"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(within(y_lower = c, y_upper = d, x_lower = a, x_upper = b))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("c", "d"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(within(y_lower = x$c, y_upper = x$d, x_lower = y$a, x_upper = y$b))
  expect_identical(by$x, c("c", "d"))
  expect_identical(by$y, c("a", "b"))
  expect_identical(by$condition, c("<=", ">="))

  by <- join_by(preceding(y = b, x = a))
  expect_identical(by$x, "a")
  expect_identical(by$y, "b")

  by <- join_by(following(y = b, x = a))
  expect_identical(by$x, "a")
  expect_identical(by$y, "b")
})

test_that("can join_by() nothing for a cross join", {
  by <- join_by()
  expect_identical(by$x, character())
  expect_identical(by$y, character())
  expect_identical(by$cross, TRUE)
})

test_that("has an informative print method", {
  expect_snapshot(join_by())
  expect_snapshot(join_by(a, b))
  expect_snapshot(join_by("a", "b"))
  expect_snapshot(join_by(a == a, b >= c))
  expect_snapshot(join_by(a == a, b >= "c"))
  expect_snapshot(join_by(a == a, preceding(b, c), following(d, e, inclusive = FALSE)))
})

test_that("has informative error messages", {
  # `=` rather than `==`
  expect_snapshot(error = TRUE, join_by(a = b))

  # Empty expression
  expect_snapshot(error = TRUE, join_by(NULL))

  # Improper rolling specification
  expect_snapshot(error = TRUE, join_by(foo(x > y)))

  # Improper separator
  expect_snapshot(error = TRUE, join_by(x == y, x ^ y))

  # Improper LHS
  expect_snapshot(error = TRUE, join_by(x + 1 == y))

  # Improper RHS
  expect_snapshot(error = TRUE, join_by(x == y + 1))

  # Garbage input
  expect_snapshot(error = TRUE, join_by(1))

  # Top level usage of `$`
  expect_snapshot(error = TRUE, join_by(x$a))

  # `$` must only contain x/y on LHS
  expect_snapshot(error = TRUE, join_by(z$a == y$b))
  expect_snapshot(error = TRUE, join_by(x$a == z$b))

  # Extra cautious check for horrible usage of `$`
  expect_snapshot(error = TRUE, join_by(`$`(x+1, y) == b))

  # Referencing the same table
  expect_snapshot(error = TRUE, join_by(x$a == x$b))
  expect_snapshot(error = TRUE, join_by(y$a == b))
  expect_snapshot(error = TRUE, join_by(between(x$a, x$a, x$b)))
  expect_snapshot(error = TRUE, join_by(within(x$a, x$b, x$a, x$b)))
  expect_snapshot(error = TRUE, join_by(overlaps(a, b, x$a, x$b)))

  # Referencing different tables in lower/upper bound pairs
  expect_snapshot(error = TRUE, join_by(between(a, x$a, y$b)))
  expect_snapshot(error = TRUE, join_by(within(x$a, y$b, y$a, y$b)))
  expect_snapshot(error = TRUE, join_by(overlaps(x$a, x$b, y$a, x$b)))

  # Too few arguments
  expect_snapshot(error = TRUE, join_by(`>`(x)))
  expect_snapshot(error = TRUE, join_by(between(x)))
  expect_snapshot(error = TRUE, join_by(within(x)))
  expect_snapshot(error = TRUE, join_by(overlaps(x)))
  expect_snapshot(error = TRUE, join_by(preceding(x)))
  expect_snapshot(error = TRUE, join_by(preceding(y = x)))
  expect_snapshot(error = TRUE, join_by(following(x)))
  expect_snapshot(error = TRUE, join_by(`$`(x) > y))

  # Arguments supplied in empty dots
  expect_snapshot(error = TRUE, join_by(preceding(x, y, TRUE)))
  expect_snapshot(error = TRUE, join_by(following(x, y, TRUE)))

  # Referencing wrong table in `preceding()` or `following()`
  expect_snapshot(error = TRUE, join_by(preceding(y$a, b)))
  expect_snapshot(error = TRUE, join_by(preceding(a, x$b)))
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
