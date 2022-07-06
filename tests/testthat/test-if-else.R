test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(if_else(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)

  expect_equal(if_else(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(if_else(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(if_else(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})

test_that("works with lists", {
  x <- list(1, 2, 3)

  expect_equal(
    if_else(c(TRUE, TRUE, FALSE), x, list(NULL)),
    list(1, 2, NULL)
  )
})

test_that("works with data frames", {
  true <- tibble(x = 1, y = 2)
  false <- tibble(x = 3, y = 4)

  expect_identical(
    if_else(c(TRUE, FALSE, NA, TRUE), true, false),
    vec_c(true, false, NA, true)
  )
})

test_that("works with vctrs rcrd types", {
  true <- new_rcrd(list(x = 1, y = 2))
  false <- new_rcrd(list(x = 3, y = 4))

  expect_identical(
    if_else(c(TRUE, FALSE, NA, TRUE), true, false),
    vec_c(true, false, NA, true)
  )
})

test_that("takes the common type of `true` and `false` (#6243)", {
  expect_identical(if_else(TRUE, 1L, 1.5), 1)

  expect_snapshot(error = TRUE, {
    if_else(TRUE, 1, "x")
  })
})

test_that("includes `missing` in the common type computation if used", {
  expect_identical(if_else(TRUE, 1L, 2L, missing = 3), 1)

  expect_snapshot(error = TRUE, {
    if_else(TRUE, 1, 2, missing = "x")
  })
})

test_that("can recycle to size 0 `condition`", {
  expect_identical(if_else(logical(), 1, 2, missing = 3), double())
})

test_that("`condition` must be logical (and isn't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    if_else(1:10, 1, 2)
  })
})

test_that("`true`, `false`, and `missing` must recycle to the size of `condition`", {
  x <- 1:3
  bad <- 1:2

  expect_snapshot(error = TRUE, {
    if_else(x < 2, bad, x)
  })
  expect_snapshot(error = TRUE, {
    if_else(x < 2, x, bad)
  })
  expect_snapshot(error = TRUE, {
    if_else(x < 2, x, x, missing = bad)
  })
})

test_that("must have empty dots", {
  expect_snapshot(error = TRUE, {
    if_else(TRUE, 1, 2, missing = 3, 4)
  })
})

test_that("`ptype` overrides the common type", {
  expect_identical(if_else(TRUE, 2, 1L, ptype = integer()), 2L)

  expect_snapshot(error = TRUE, {
    if_else(TRUE, 1L, 2.5, ptype = integer())
  })
})

test_that("`size` overrides the `condition` size", {
  expect_identical(if_else(c(TRUE, FALSE), 1, 2, size = 2), c(1, 2))

  # Note that `condition` is used as the name in the error message
  expect_snapshot(error = TRUE, {
    if_else(TRUE, 1, 2, size = 2)
  })
})
