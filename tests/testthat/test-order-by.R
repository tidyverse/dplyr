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
