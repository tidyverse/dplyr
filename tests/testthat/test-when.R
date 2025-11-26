test_that("all possible variations of each combination are right", {
  N <- NA

  expect_identical(when_all(T, T, na_rm = FALSE), T)
  expect_identical(when_all(T, F, na_rm = FALSE), F)
  expect_identical(when_all(T, N, na_rm = FALSE), N)
  expect_identical(when_all(F, T, na_rm = FALSE), F)
  expect_identical(when_all(F, F, na_rm = FALSE), F)
  expect_identical(when_all(F, N, na_rm = FALSE), F)
  expect_identical(when_all(N, T, na_rm = FALSE), N)
  expect_identical(when_all(N, F, na_rm = FALSE), F)
  expect_identical(when_all(N, N, na_rm = FALSE), N)

  expect_identical(when_all(T, T, na_rm = TRUE), T)
  expect_identical(when_all(T, F, na_rm = TRUE), F)
  expect_identical(when_all(T, N, na_rm = TRUE), T)
  expect_identical(when_all(F, T, na_rm = TRUE), F)
  expect_identical(when_all(F, F, na_rm = TRUE), F)
  expect_identical(when_all(F, N, na_rm = TRUE), F)
  expect_identical(when_all(N, T, na_rm = TRUE), T)
  expect_identical(when_all(N, F, na_rm = TRUE), F)
  expect_identical(when_all(N, N, na_rm = TRUE), T)

  expect_identical(when_any(T, T, na_rm = FALSE), T)
  expect_identical(when_any(T, F, na_rm = FALSE), T)
  expect_identical(when_any(T, N, na_rm = FALSE), T)
  expect_identical(when_any(F, T, na_rm = FALSE), T)
  expect_identical(when_any(F, F, na_rm = FALSE), F)
  expect_identical(when_any(F, N, na_rm = FALSE), N)
  expect_identical(when_any(N, T, na_rm = FALSE), T)
  expect_identical(when_any(N, F, na_rm = FALSE), N)
  expect_identical(when_any(N, N, na_rm = FALSE), N)

  expect_identical(when_any(T, T, na_rm = TRUE), T)
  expect_identical(when_any(T, F, na_rm = TRUE), T)
  expect_identical(when_any(T, N, na_rm = TRUE), T)
  expect_identical(when_any(F, T, na_rm = TRUE), T)
  expect_identical(when_any(F, F, na_rm = TRUE), F)
  expect_identical(when_any(F, N, na_rm = TRUE), F)
  expect_identical(when_any(N, T, na_rm = TRUE), T)
  expect_identical(when_any(N, F, na_rm = TRUE), F)
  expect_identical(when_any(N, N, na_rm = TRUE), F)
})

test_that("empty case works", {
  expect_identical(when_any(), logical())
  expect_identical(when_all(), logical())
})

test_that("`size` influences the empty case", {
  expect_identical(when_any(size = 1), FALSE)
  expect_identical(when_all(size = 1), TRUE)
})

test_that("no recycling is performed!", {
  # On the vctrs side we decided recycling doesn't
  # make much sense in these functions

  expect_snapshot(error = TRUE, {
    when_any(TRUE, c(TRUE, FALSE))
  })
  expect_snapshot(error = TRUE, {
    when_all(TRUE, c(TRUE, FALSE))
  })

  expect_snapshot(error = TRUE, {
    when_any(TRUE, size = 2)
  })
  expect_snapshot(error = TRUE, {
    when_all(TRUE, size = 2)
  })
})

test_that("inputs must be strictly logical vectors", {
  # Not cast to logical
  expect_snapshot(error = TRUE, {
    when_any(1)
  })
  expect_snapshot(error = TRUE, {
    when_all(1)
  })

  # Not a 1D array of logical
  expect_snapshot(error = TRUE, {
    when_any(array(TRUE))
  })
  expect_snapshot(error = TRUE, {
    when_all(array(TRUE))
  })

  # Not a classed logical
  expect_snapshot(error = TRUE, {
    when_any(structure(TRUE, class = "foo"))
  })
  expect_snapshot(error = TRUE, {
    when_all(structure(TRUE, class = "foo"))
  })

  # Extraneous attributes are fine
  expect_identical(when_any(structure(TRUE, foo = "bar")), TRUE)
  expect_identical(when_all(structure(TRUE, foo = "bar")), TRUE)
})

test_that("`...` can't be named", {
  # This is why we can have non `.` prefixed arguments
  expect_snapshot(error = TRUE, {
    when_any(x = TRUE)
  })
  expect_snapshot(error = TRUE, {
    when_all(x = TRUE)
  })
})

test_that("`na_rm` is validated", {
  expect_snapshot(error = TRUE, {
    when_any(na_rm = "x")
  })
  expect_snapshot(error = TRUE, {
    when_all(na_rm = "x")
  })
})

test_that("`size` is validated", {
  # Good enough, just don't want to crash
  expect_snapshot(error = TRUE, {
    when_any(size = "x")
  })
  expect_snapshot(error = TRUE, {
    when_all(size = "x")
  })
})
