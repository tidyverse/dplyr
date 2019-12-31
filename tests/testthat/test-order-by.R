test_that("order_by() gives useful error messages", {
  verify_output(test_path("test-order-by-error.txt"), {
    order_by(mtcars, 10)

    order_by(mtcars, cyl)
  })
})
