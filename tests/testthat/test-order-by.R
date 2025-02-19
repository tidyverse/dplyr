test_that("order_by() gives useful error messages", {
  expect_snapshot({
    (expect_error(order_by(mtcars, 10)))
    (expect_error(order_by(mtcars, cyl)))
  })
})

test_that("`with_order()` works with data frame `order_by` (#6334)", {
  x <- 1:3
  order_by <- tibble(a = c(1, 1, 2), b = c(2, 1, 1))

  expect_identical(with_order(order_by, lag, x), c(2L, NA, 1L))
})

test_that("`with_order()` requires `order_by` and `x` to be the same size", {
  expect_snapshot(error = TRUE, {
    with_order(1:2, identity, 1:3)
  })
})

test_that("order_by() returns correct value", {
  expected <- int(15, 14, 12, 9, 5)
  expect_identical(order_by(5:1, cumsum(1:5)), expected)

  x <- 5:1
  y <- 1:5
  expect_identical(order_by(x, cumsum(y)), expected)
})

test_that("order_by() works in arbitrary envs (#2297)", {
  env <- child_env("base")
  expect_equal(
    with_env(env, dplyr::order_by(5:1, cumsum(1:5))),
    rev(cumsum(rev(1:5)))
  )
  expect_equal(
    order_by(5:1, cumsum(1:5)),
    rev(cumsum(rev(1:5)))
  )
})

test_that("order_by() give meaningful errors", {
  expect_snapshot({
    (expect_error(order_by(NULL, 1L)))
  })
})
