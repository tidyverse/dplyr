test_that("order_by() gives useful error messages", {
  expect_snapshot({
    (expect_error(order_by(mtcars, 10)))
    (expect_error(order_by(mtcars, cyl)))
  })
})
