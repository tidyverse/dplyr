context("utils")

test_that("quo_is_variable_reference handles .data",{
  expect_true(quo_is_variable_reference(quo(x)))
  expect_true(quo_is_variable_reference(quo(.data$x)))
  expect_true(quo_is_variable_reference(quo(.data[["x"]])))
  quo <- new_quosure(quote(.data[[identity("x")]]))
  expect_false(quo_is_variable_reference(quo))
})

test_that("check_pkg() give meaningful error messages", {
  verify_output(test_path("test-utils-errors.txt"), {
    dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz")
    dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz", install = FALSE)
  })
})
