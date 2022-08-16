# ------------------------------------------------------------------------------
# nth()

test_that("nth works with lists and uses `vec_slice2()` to return elements (#6331)", {
  # We'd like to use `vec_slice()` everywhere, but it breaks too many revdeps
  # that rely on `nth(<list>)` returning list elements
  x <- list(1, 2, 3:5)

  expect_equal(nth(x, 1), 1)
  expect_equal(nth(x, 3), 3:5)
})

test_that("nth `default` for lists defaults to `NULL` since it uses `vec_slice2()`", {
  expect_null(nth(list(1), 2))
  expect_null(nth(list(), 1))
})

test_that("nth `default` for lists can be anything", {
  # Because list elements can be anything
  x <- list(1, 2)

  default <- environment()
  expect_identical(nth(x, 3, default = default), default)

  default <- 1:3
  expect_identical(nth(x, 3, default = default), default)
})

test_that("nth treats list-of like lists", {
  x <- list_of(1, 2, c(3, 4))

  expect_identical(nth(x, 3), c(3, 4))
  expect_identical(nth(x, 4), NULL)

  # Not particularly strict about `default` here,
  # even though `list_of()` elements are typed
  expect_identical(nth(x, 4, default = "x"), "x")
})

test_that("nth works with data frames and always returns a single row", {
  x <- tibble(x = 1:3, y = 4:6)

  expect_identical(nth(x, 1), tibble(x = 1L, y = 4L))
  expect_identical(nth(x, 4), tibble(x = NA_integer_, y = NA_integer_))
  expect_identical(nth(x, 4, default = tibble(x = 0, y = 0)), tibble(x = 0L, y = 0L))
})

test_that("nth works with rcrds", {
  x <- new_rcrd(list(x = 1:3, y = 4:6))

  expect_identical(nth(x, 1), vec_slice(x, 1))
  expect_identical(nth(x, 4), vec_init(x))
  expect_identical(nth(x, 4, default = x[2]), x[2])
})

test_that("drops names, because it uses `vec_slice2()`", {
  x <- c(a = 1, b = 2)
  expect_named(nth(x, 2), NULL)
})

test_that("negative values index from end", {
  x <- 1:5

  expect_equal(nth(x, -1), 5L)
  expect_equal(nth(x, -3), 3L)
})

test_that("indexing past ends returns default value", {
  expect_equal(nth(1:4, 5), NA_integer_)
  expect_equal(nth(1:4, -5), NA_integer_)
  expect_equal(nth(1:4, -10), NA_integer_)
  expect_equal(nth(1:4, -10, default = 6L), 6L)
})

test_that("gets corner case indexing correct", {
  expect_identical(nth(1:4, -5), NA_integer_)
  expect_identical(nth(1:4, -4), 1L)
  expect_identical(nth(1:4, -3), 2L)

  expect_identical(nth(1:4, -1), 4L)
  expect_identical(nth(1:4, 0), NA_integer_)
  expect_identical(nth(1:4, 1), 1L)

  expect_identical(nth(1:4, 3), 3L)
  expect_identical(nth(1:4, 4), 4L)
  expect_identical(nth(1:4, 5), NA_integer_)
})

test_that("`order_by` can be used to alter the order", {
  expect_identical(nth(1:5, n = 1L, order_by = 5:1), 5L)
  expect_identical(nth(as.list(1:5), n = 1L, order_by = 5:1), 5L)
})

test_that("can use a data frame as `order_by`", {
  x <- 1:3
  order_by <- tibble(a = c(1, 1, 2), b = c(2, 1, 0))

  expect_identical(nth(x, 1, order_by = order_by), 2L)
  expect_identical(nth(x, 2, order_by = order_by), 1L)
})

test_that("`na_rm` can be used to drop missings before selecting the value (#6242)", {
  x <- c(NA, 4, 10, NA, 5, NA)

  expect_identical(nth(x, 1, na_rm = TRUE), 4)
  expect_identical(nth(x, -1, na_rm = TRUE), 5)
  expect_identical(nth(x, 3, na_rm = TRUE), 5)
})

test_that("`na_rm` removes `NULL` list elements", {
  x <- list(1:3, NULL, 4, integer(), NULL, NULL)

  expect_identical(nth(x, 2, na_rm = TRUE), 4)
  expect_identical(nth(x, -1, na_rm = TRUE), integer())
})

test_that("`na_rm` can generate OOB selections, resulting in `default`", {
  # Removes some values
  x <- c(NA, FALSE, NA)
  expect_identical(nth(x, 2, default = TRUE, na_rm = TRUE), TRUE)

  # Removes everything
  x <- c(NA, NA, NA)
  expect_identical(nth(x, 1, default = TRUE, na_rm = TRUE), TRUE)
  expect_identical(nth(x, -2, default = TRUE, na_rm = TRUE), TRUE)
})

test_that("`na_rm` slices `order_by` as well", {
  x <- c(NA, 4, 10, NA, 5, NA)
  o <- c(2, 1, 3, 1, 1, 0)

  expect_identical(nth(x, 1, order_by = o, na_rm = TRUE), 4)
  expect_identical(nth(x, -1, order_by = o, na_rm = TRUE), 10)
  expect_identical(nth(x, 2, order_by = o, na_rm = TRUE), 5)
  expect_identical(nth(x, 3, order_by = o, na_rm = TRUE), 10)
})

test_that("`na_rm` is validated", {
  expect_snapshot(error = TRUE, {
    nth(1, 1, na_rm = 1)
  })
  expect_snapshot(error = TRUE, {
    nth(1, 1, na_rm = c(TRUE, FALSE))
  })
})

test_that("`default` must be size 1 (when not used with lists)", {
  expect_snapshot(error = TRUE, {
    nth(1L, n = 2L, default = 1:2)
  })
})

test_that("`default` is cast to the type of `x` (when not used with lists)", {
  expect_snapshot(error = TRUE, {
    nth("x", 2, default = 2)
  })
})

test_that("`n` is validated (#5466)", {
  expect_snapshot(error = TRUE, {
    nth(1:10, n = "x")
  })
  expect_snapshot(error = TRUE, {
    nth(1:10, n = 1:2)
  })
})

test_that("`x` must be a vector", {
  expect_snapshot(error = TRUE, {
    nth(environment(), 1L)
  })
})

test_that("`order_by` must be the same size as `x`", {
  expect_snapshot(error = TRUE, {
    nth(1:5, n = 1L, order_by = 1:2)
  })

  # Ensure that this is checked before `default` is early returned
  expect_snapshot(error = TRUE, {
    nth(1:5, n = 6L, order_by = 1:2)
  })
})

# ------------------------------------------------------------------------------
# first()

test_that("`first()` selects the first value", {
  expect_identical(first(1:5), 1L)
})

test_that("`first()` uses default value for 0 length vectors", {
  expect_equal(first(logical()), NA)
  expect_equal(first(integer()), NA_integer_)
  expect_equal(first(numeric()), NA_real_)
  expect_equal(first(character()), NA_character_)
})

test_that("`first()` uses `NULL` default for 0 length lists", {
  expect_identical(first(list()), NULL)
})

test_that("`first()` uses default value for 0 length augmented vectors", {
  fc <- factor("a")[0]
  dt <- Sys.Date()[0]
  tm <- Sys.time()[0]

  expect_equal(first(fc), vec_init(fc))
  expect_equal(first(dt), vec_init(dt))
  expect_equal(first(tm), vec_init(tm))
})

test_that("`first()` returns list elements", {
  expect_identical(first(list(2:3, 4:5)), 2:3)
})

test_that("`first()` respects `na_rm`", {
  x <- c(NA, NA, 2, 3)
  expect_identical(first(x, na_rm = TRUE), 2)
})

# ------------------------------------------------------------------------------
# last()

test_that("`last()` selects the last value", {
  expect_identical(last(1:5), 5L)
})

test_that("`last()` returns list elements", {
  expect_identical(last(list(2:3, 4:5)), 4:5)
})

test_that("`last()` respects `na_rm`", {
  x <- c(2, 3, NA, NA)
  expect_identical(last(x, na_rm = TRUE), 3)
})
