# ------------------------------------------------------------------------------
# quo_is_variable_reference()

test_that("quo_is_variable_reference handles .data", {
  expect_true(quo_is_variable_reference(quo(x)))
  expect_true(quo_is_variable_reference(quo(.data$x)))
  expect_true(quo_is_variable_reference(quo(.data[["x"]])))
  quo <- new_quosure(quote(.data[[identity("x")]]))
  expect_false(quo_is_variable_reference(quo))
})

# ------------------------------------------------------------------------------
# list_assign_by_name()

test_that("`list_assign_by_name()` matches assigning one name at a time", {
  assign_each <- function(x, names, values) {
    for (i in seq_along(values)) {
      x[[names[[i]]]] <- values[[i]]
    }
    x
  }
  expect_same <- function(x, names, values) {
    expect_identical(
      list_assign_by_name(x, names, values),
      assign_each(x, names, values)
    )
  }

  x <- list(a = 1, b = 2, c = 3)

  # Replace, append, and remove
  expect_same(x, c("b", "d", "a", "e"), list(20, 4, NULL, list(5)))
  expect_same(x, c("z", "c"), list(NULL, NULL))
  expect_same(x, character(), list())
  expect_same(list(), c("a", "b"), list(1, 2))
  expect_same(set_names(list(), character()), character(), list())

  # Repeated names, where the order of the assignments matters
  expect_same(x, c("a", "a", "d", "d"), list(NULL, 10, 4, 40))
  expect_same(list(a = 1, a = 2), c("a", "b"), list(10, NULL))
})

# ------------------------------------------------------------------------------
# list_flatten()

test_that("`list_flatten()` is a no-op on flattened lists", {
  x <- list(1, 2)
  expect_identical(list_flatten(x), x)
})

test_that("`list_flatten()` flattens list elements", {
  x <- list(list(1, 2), 3, list(4))
  expect_identical(list_flatten(x), list(1, 2, 3, 4))
})

test_that("`list_flatten()` doesn't try to be generic", {
  my_list <- function(...) structure(list(...), class = c("my_list", "list"))

  x <- my_list(list(1, 2), 3, my_list(4))
  expect_identical(list_flatten(x), list(1, 2, 3, 4))

  # The no-op case returns a bare list too
  x <- my_list(1, 2)
  expect_identical(list_flatten(x), list(1, 2))
})

test_that("`list_flatten()` only retains inner names of flattened elements", {
  x <- list(a = list(1, b = 2), 3, list(d = 4), e = 5, f = list(1))
  expect_identical(list_flatten(x), list(1, b = 2, 3, d = 4, e = 5, 1))
})

test_that("`list_flatten()` can work recursively", {
  x <- list(list(list(1, 2), 3), 4)

  # Not by default
  expect_identical(list_flatten(x), list(list(1, 2), 3, 4))

  expect_identical(list_flatten(x, recursive = TRUE), list(1, 2, 3, 4))
})

test_that("recursive `list_flatten()` handles names correctly", {
  x <- list(a = list(b = list(1), c = list(d = 2), 3, e = 4), f = 5)

  expect_identical(
    list_flatten(x, recursive = TRUE),
    list(1, d = 2, 3, e = 4, f = 5)
  )
})

test_that("`list_flatten()` accepts a predicate `fn` to selectively flatten", {
  is_flattenable <- function(x) !is_named(x)

  x <- list(
    a = list(list(1), list(b = 2), 3),
    c = 4,
    d = list(e = 5),
    f = list(6)
  )

  expect_identical(
    list_flatten(x, fn = is_flattenable),
    list(list(1), list(b = 2), 3, c = 4, d = list(e = 5), 6)
  )
  expect_identical(
    list_flatten(x, fn = is_flattenable, recursive = TRUE),
    list(1, list(b = 2), 3, c = 4, d = list(e = 5), 6)
  )
})
