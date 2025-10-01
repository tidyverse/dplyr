test_that("non-missing scalar replaces all missing values", {
  x <- c(NA, 1)
  expect_equal(coalesce(x, 1), c(1, 1))
})

test_that("coerces to common type", {
  expect_identical(coalesce(NA, 1), 1)

  f <- factor("x", levels = c("x", "y"))
  expect_identical(coalesce(NA, f), f)
})

test_that("inputs are recycled to their common size", {
  expect_identical(coalesce(1, c(2, 3)), c(1, 1))
  expect_identical(coalesce(1, 2:3), c(1, 1))
})

test_that("finds non-missing values in multiple positions", {
  x1 <- c(1L, NA, NA)
  x2 <- c(NA, 2L, NA)
  x3 <- c(NA, NA, 3L)

  expect_equal(coalesce(x1, x2, x3), 1:3)
})

test_that("coalesce() gives meaningful error messages", {
  expect_snapshot(error = TRUE, {
    coalesce(1:2, 1:3)
  })
  expect_snapshot(error = TRUE, {
    coalesce(1:2, letters[1:2])
  })
})

test_that("coalesce() supports one-dimensional arrays (#5557)", {
  x <- array(1:10)
  out <- coalesce(x, 0L)
  expect_identical(out, x)
})

test_that("only updates entirely missing matrix rows", {
  x <- c(1, NA, NA, NA)
  x <- matrix(x, nrow = 2, byrow = TRUE)

  y <- c(2, 2, NA, 1)
  y <- matrix(y, nrow = 2, byrow = TRUE)

  expect <- c(1, NA, NA, 1)
  expect <- matrix(expect, nrow = 2, byrow = TRUE)

  expect_identical(coalesce(x, y), expect)
})

test_that("only updates entirely missing data frame rows", {
  x <- tibble(x = c(1, NA), y = c(NA, NA))
  y <- tibble(x = c(2, NA), y = c(TRUE, TRUE))

  expect <- tibble(x = c(1, NA), y = c(NA, TRUE))

  expect_identical(coalesce(x, y), expect)
})

test_that("only updates entirely missing rcrd observations", {
  x <- new_rcrd(list(x = c(1, NA), y = c(NA, NA)))
  y <- new_rcrd(list(x = c(2, NA), y = c(TRUE, TRUE)))

  expect <- new_rcrd(list(x = c(1, NA), y = c(NA, TRUE)))

  expect_identical(coalesce(x, y), expect)
})

test_that("`.ptype` overrides the common type (r-lib/funs#64)", {
  x <- c(1L, NA)
  expect_identical(coalesce(x, 99, .ptype = x), c(1L, 99L))
})

test_that("`.size` overrides the common size", {
  x <- 1L

  expect_snapshot(error = TRUE, {
    coalesce(x, 1:2, .size = vec_size(x))
  })
})

test_that("can't be empty", {
  expect_snapshot(error = TRUE, {
    coalesce()
  })
})

test_that("must have at least one non-`NULL` vector", {
  expect_snapshot(error = TRUE, {
    coalesce(NULL, NULL)
  })
})

test_that("`NULL`s are discarded (r-lib/funs#80)", {
  expect_identical(
    coalesce(c(1, NA, NA), NULL, c(1, 2, NA), NULL, 3),
    c(1, 2, 3)
  )
})

test_that("works with multiple scalars", {
  expect_identical(
    coalesce(c(1, NA), 2, 3),
    c(1, 2)
  )
})

test_that("works with trailing `NA`", {
  # Not promoted to `default`
  expect_identical(
    coalesce(c(1, NA), NA),
    c(1, NA)
  )
})

test_that("resulting names come from all inputs", {
  expect_named(
    coalesce(
      c(x = 1, y = NA),
      c(a = 3, b = 4)
    ),
    c("x", "b")
  )

  # No name if nothing stops the coalesce
  expect_named(
    coalesce(
      c(x = 1, y = NA),
      c(a = 3, b = NA)
    ),
    c("x", "")
  )

  # Unused inputs still force named output.
  # "Common names" principle, like common type or size.
  expect_named(
    coalesce(
      c(1, 2),
      c(a = 3, b = 4)
    ),
    c("", "")
  )
  expect_named(
    coalesce(
      c(1, 2),
      c(a = NA_real_)
    ),
    c("", "")
  )

  # Size 1 default name is recycled if used
  expect_named(
    coalesce(
      c(a = 1, b = NA, c = 2, d = NA),
      c(e = 0)
    ),
    c("a", "e", "c", "e")
  )
  expect_named(
    coalesce(
      c(a = 1, b = NA, c = 2, d = NA),
      c(e = NA)
    ),
    c("a", "", "c", "")
  )

  # With multiple scalars that force namedness
  expect_named(
    coalesce(c(1, NA), 2, c(a = 3)),
    c("", "")
  )
  expect_named(
    coalesce(c(1, NA), c(a = 2), 3),
    c("", "a")
  )
})

test_that("inputs must be vectors", {
  expect_snapshot(error = TRUE, {
    coalesce(1, environment())
  })
})

test_that("names in error messages are indexed correctly", {
  expect_snapshot(error = TRUE, {
    coalesce(1, "x")
  })
  expect_snapshot(error = TRUE, {
    coalesce(1, y = "x")
  })
  expect_snapshot(error = TRUE, {
    coalesce(1:2, 1:3)
  })
  expect_snapshot(error = TRUE, {
    coalesce(1:2, y = 1:3)
  })

  # With `NULL`s, which get "dropped"
  expect_snapshot(error = TRUE, {
    coalesce(1, NULL, "x")
  })
  expect_snapshot(error = TRUE, {
    coalesce(1, NULL, y = "x")
  })
  expect_snapshot(error = TRUE, {
    coalesce(1:2, NULL, 1:3)
  })
  expect_snapshot(error = TRUE, {
    coalesce(1:2, NULL, y = 1:3)
  })
})
