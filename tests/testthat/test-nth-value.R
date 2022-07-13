# ------------------------------------------------------------------------------
# nth()

test_that("nth works with lists and always returns an object of size 1", {
  x <- list(1, 2, 3)

  expect_equal(nth(x, 1), list(1))
  expect_equal(nth(x, 4), list(NULL))
  expect_equal(nth(x, 4, default = list(0)), list(0))
})

test_that("nth works with data frames and always returns an object of size 1", {
  x <- tibble(x = 1:3, y = 4:6)

  expect_identical(nth(x, 1), vec_slice(x, 1))
  expect_identical(nth(x, 4), tibble(x = NA_integer_, y = NA_integer_))
  expect_identical(nth(x, 4, default = tibble(x = 0, y = 0)), tibble(x = 0L, y = 0L))
})

test_that("nth works with rcrds", {
  x <- new_rcrd(list(x = 1:3, y = 4:6))

  expect_identical(nth(x, 1), vec_slice(x, 1))
  expect_identical(nth(x, 4), vec_init(x))
  expect_identical(nth(x, 4, default = x[2]), x[2])
})

test_that("retains names, because it uses `vec_slice()` to always return an object of size 1", {
  x <- c(a = 1, b = 2)
  expect_named(nth(x, 2), "b")
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
})

test_that("can use a data frame as `order_by`", {
  x <- 1:3
  order_by <- tibble(a = c(1, 1, 2), b = c(2, 1, 0))

  expect_identical(nth(x, 1, order_by = order_by), 2L)
  expect_identical(nth(x, 2, order_by = order_by), 1L)
})

test_that("`default` must be size 1", {
  expect_snapshot(error = TRUE, {
    nth(1L, n = 2L, default = 1:2)
  })
})

test_that("`default` is cast to the type of `x`", {
  expect_snapshot(error = TRUE, {
    nth(list(1), 2, default = 2)
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
  expect_equal(first(list()), list(NULL))
})

test_that("`first()` uses default value for 0 length augmented vectors", {
  fc <- factor("a")[0]
  dt <- Sys.Date()[0]
  tm <- Sys.time()[0]

  expect_equal(first(fc), vec_init(fc))
  expect_equal(first(dt), vec_init(dt))
  expect_equal(first(tm), vec_init(tm))
})

# ------------------------------------------------------------------------------
# last()

test_that("`last()` selects the last value", {
  expect_identical(last(1:5), 5L)
})
