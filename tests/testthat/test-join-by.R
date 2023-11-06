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

test_that("works with `closest()`", {
  by <- join_by(x == y, closest(a >= b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "max"))
  expect_identical(by$condition, c("==", ">="))

  by <- join_by(x == y, closest(a > b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "max"))
  expect_identical(by$condition, c("==", ">"))

  by <- join_by(x == y, closest(a <= b))

  expect_identical(by$x, c("x", "a"))
  expect_identical(by$y, c("y", "b"))
  expect_identical(by$filter, c("none", "min"))
  expect_identical(by$condition, c("==", "<="))

  by <- join_by(x == y, closest(a < b))

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
  by1 <- join_by("a", "b" == "c", closest("d" >= "e"))
  by2 <- join_by(a, b  == c, closest(d >= e))

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

test_that("`closest()` works with explicit referencing", {
  by <- join_by(closest(y$a <= x$b), closest(y$a > x$b))
  expect_identical(by$x, c("b", "b"))
  expect_identical(by$y, c("a", "a"))
  expect_identical(by$filter, c("max", "min"))
  expect_identical(by$condition, c(">=", "<"))
})

test_that("between conditions expand correctly", {
  by <- join_by(between(a, b, c))
  expect_identical(by$x, c("a", "a"))
  expect_identical(by$y, c("b", "c"))

  by <- join_by(between(y$a, x$b, x$c))
  expect_identical(by$x, c("b", "c"))
  expect_identical(by$y, c("a", "a"))

  by <- join_by(between(a, b, c, bounds = "[]"))
  expect_identical(by$condition, c(">=", "<="))
  by <- join_by(between(a, b, c, bounds = "[)"))
  expect_identical(by$condition, c(">=", "<"))
  by <- join_by(between(a, b, c, bounds = "(]"))
  expect_identical(by$condition, c(">", "<="))
  by <- join_by(between(a, b, c, bounds = "()"))
  expect_identical(by$condition, c(">", "<"))

  by <- join_by(between(y$a, x$b, x$c, bounds = "[]"))
  expect_identical(by$condition, c("<=", ">="))
  by <- join_by(between(y$a, x$b, x$c, bounds = "[)"))
  expect_identical(by$condition, c("<=", ">"))
  by <- join_by(between(y$a, x$b, x$c, bounds = "(]"))
  expect_identical(by$condition, c("<", ">="))
  by <- join_by(between(y$a, x$b, x$c, bounds = "()"))
  expect_identical(by$condition, c("<", ">"))
})

test_that("within conditions expand correctly", {
  by <- join_by(within(a, b, c, d))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("c", "d"))
  expect_identical(by$condition, c(">=", "<="))

  by <- join_by(within(y$a, y$b, x$b, x$c))
  expect_identical(by$x, c("b", "c"))
  expect_identical(by$y, c("a", "b"))
  expect_identical(by$condition, c("<=", ">="))
})

test_that("overlaps conditions expand correctly", {
  by <- join_by(overlaps(a, b, c, d))
  expect_identical(by$x, c("a", "b"))
  expect_identical(by$y, c("d", "c"))

  by <- join_by(overlaps(y$a, y$b, x$b, x$c))
  expect_identical(by$x, c("c", "b"))
  expect_identical(by$y, c("a", "b"))

  by <- join_by(overlaps(a, b, c, d, bounds = "[]"))
  expect_identical(by$condition, c("<=", ">="))
  by <- join_by(overlaps(a, b, c, d, bounds = "[)"))
  expect_identical(by$condition, c("<", ">"))
  by <- join_by(overlaps(a, b, c, d, bounds = "(]"))
  expect_identical(by$condition, c("<", ">"))
  by <- join_by(overlaps(a, b, c, d, bounds = "()"))
  expect_identical(by$condition, c("<", ">"))

  by <- join_by(overlaps(y$a, y$b, x$b, x$c, bounds = "[]"))
  expect_identical(by$condition, c(">=", "<="))
  by <- join_by(overlaps(y$a, y$b, x$b, x$c, bounds = "[)"))
  expect_identical(by$condition, c(">", "<"))
  by <- join_by(overlaps(y$a, y$b, x$b, x$c, bounds = "(]"))
  expect_identical(by$condition, c(">", "<"))
  by <- join_by(overlaps(y$a, y$b, x$b, x$c, bounds = "()"))
  expect_identical(by$condition, c(">", "<"))
})

test_that("between / overlaps / within / closest can use named arguments", {
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

  by <- join_by(closest(expr = a > b))
  expect_identical(by$x, "a")
  expect_identical(by$y, "b")
})

test_that("joining by nothing is an error", {
  expect_snapshot(error = TRUE, {
    join_by()
  })
})

test_that("can pass `...` on to wrapped `join_by()`", {
  fn <- function(...) {
    join_by(...)
  }
  fn2 <- function(x) {
    fn({{x}} == y)
  }

  expect_identical(fn(x == y, a <= b), join_by(x == y, a <= b))
  expect_identical(fn2(a), join_by(a == y))
})

test_that("can wrap `join_by()` and use embracing to inject columns (#6469)", {
  fn <- function(x) {
    join_by({{x}} == y)
  }
  expect_identical(fn("foo"), join_by("foo" == y))

  # Expression substitution, not quosure evaluation
  a <- "foo"
  expect_identical(fn(a), join_by(a == y))

  # But you can inline with `!!`
  expect_identical(fn(!!a), join_by("foo" == y))

  fn <- function(x, top) {
    join_by(between({{x}}, lower, {{top}}))
  }
  expect_identical(fn(x, y), join_by(between(x, lower, y)))
})

test_that("can wrap `join_by()` and use embracing to inject expressions", {
  fn <- function(expr) {
    join_by({{expr}}, a <= b)
  }
  expect_identical(fn(a == b), join_by(a == b, a <= b))
})

test_that("nicely catches required missing arguments when wrapped", {
  fn <- function(x, y) {
    join_by({{x}} == {{y}})
  }
  expect_snapshot(error = TRUE, fn(a))
})

test_that("allows for namespaced helpers (#6838)", {
  # Captures namespaced expression for printing
  expect_snapshot(join_by(dplyr::between(x, left, right)))
  expect_snapshot(join_by(dplyr::within(xl, xu, yl, yu)))
  expect_snapshot(join_by(dplyr::overlaps(xl, xu, yl, yu)))
  expect_snapshot(join_by(dplyr::closest(x < y)))

  # Underlying values are otherwise the same as non-namespaced version
  by <- join_by(dplyr::between(x, left, right))
  reference <- join_by(between(x, left, right))

  expect_identical(by$condition, reference$condition)
  expect_identical(by$filter, reference$filter)
  expect_identical(by$x, reference$x)
  expect_identical(by$y, reference$y)
})

test_that("has an informative print method", {
  expect_snapshot(join_by(a, b))
  expect_snapshot(join_by("a", "b"))
  expect_snapshot(join_by(a == a, b >= c))
  expect_snapshot(join_by(a == a, b >= "c"))
  expect_snapshot(join_by(a == a, closest(b >= c), closest(d < e)))
})

test_that("has informative error messages", {
  # `=` rather than `==`
  expect_snapshot(error = TRUE, join_by(a = b))

  # Empty expression
  expect_snapshot(error = TRUE, join_by(NULL))

  # Improper helper specification
  expect_snapshot(error = TRUE, join_by(foo(x > y)))

  # Improper separator
  expect_snapshot(error = TRUE, join_by(x == y, x ^ y))

  # Improper LHS
  expect_snapshot(error = TRUE, join_by(x + 1 == y))

  # Improper RHS
  expect_snapshot(error = TRUE, join_by(x == y + 1))

  # Garbage input
  expect_snapshot(error = TRUE, join_by(1))

  # Call with non-symbol first element
  expect_snapshot(error = TRUE, join_by(1()))

  # Namespace prefixed helper with non-dplyr namespace
  # (typo or re-export, which currently isn't allowed)
  expect_snapshot(error = TRUE, join_by(dplyrr::between(x, left, right)))

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
  expect_snapshot(error = TRUE, join_by(closest(x$a >= x$b)))

  # Referencing different tables in lower/upper bound pairs
  expect_snapshot(error = TRUE, join_by(between(a, x$a, y$b)))
  expect_snapshot(error = TRUE, join_by(within(x$a, y$b, y$a, y$b)))
  expect_snapshot(error = TRUE, join_by(overlaps(x$a, x$b, y$a, x$b)))

  # Too few arguments
  expect_snapshot(error = TRUE, join_by(`>`(x)))
  expect_snapshot(error = TRUE, join_by(between(x)))
  expect_snapshot(error = TRUE, join_by(within(x)))
  expect_snapshot(error = TRUE, join_by(overlaps(x)))
  expect_snapshot(error = TRUE, join_by(closest()))
  expect_snapshot(error = TRUE, join_by(`$`(x) > y))

  # Too many arguments
  expect_snapshot(error = TRUE, join_by(closest(a >= b, 1)))

  # `==` in `closest()`
  expect_snapshot(error = TRUE, join_by(closest(a == b)))

  # Non-expression in `closest()`
  expect_snapshot(error = TRUE, join_by(closest(x)))
  expect_snapshot(error = TRUE, join_by(closest(1)))

  # Invalid expression in `closest()`
  expect_snapshot(error = TRUE, join_by(closest(x + y)))

  # Invalid `bounds` in `between()` and `overlaps()`
  expect_snapshot(error = TRUE, join_by(between(x, lower, upper, bounds = 1)))
  expect_snapshot(error = TRUE, join_by(between(x, lower, upper, bounds = "a")))
  expect_snapshot(error = TRUE, join_by(overlaps(x, y, lower, upper, bounds = 1)))
  expect_snapshot(error = TRUE, join_by(overlaps(x, y, lower, upper, bounds = "a")))

  # Non-empty dots in `between()` and `overlaps()`
  expect_snapshot(error = TRUE, join_by(between(x, lower, upper, foo = 1)))
  expect_snapshot(error = TRUE, join_by(overlaps(x, y, lower, upper, foo = 1)))
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

  # Works with names that need backticks
  expect_snapshot(by <- join_by_common(c("_x", "foo bar"), c("_x", "foo bar")))

  # No common variables error
  expect_snapshot(error = TRUE, join_by_common(c("x", "y"), c("w", "z")))
})
