test_that("quo_is_variable_reference handles .data",{
  expect_true(quo_is_variable_reference(quo(x)))
  expect_true(quo_is_variable_reference(quo(.data$x)))
  expect_true(quo_is_variable_reference(quo(.data[["x"]])))
  quo <- new_quosure(quote(.data[[identity("x")]]))
  expect_false(quo_is_variable_reference(quo))
})
