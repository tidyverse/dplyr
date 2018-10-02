context("utils")

test_that("check_pkg() gives correct error message", {
  expect_error(
    dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz"),
    'The `__foobarbaz__` package is required to foobar a baz.\nPlease install it with `install.packages("`__foobarbaz__`")`',
    fixed = TRUE
  )
  expect_error(
    dplyr:::check_pkg("`__foobarbaz__`", "foobar a baz", install = FALSE),
    "The `__foobarbaz__` package is required to foobar a baz."
  )
})

test_that("quo_is_variable_reference handles .data",{
  expect_true(quo_is_variable_reference(quo(x)))
  expect_true(quo_is_variable_reference(quo(.data$x)))
  expect_true(quo_is_variable_reference(quo(.data[["x"]])))
  quo <- new_quosure(quote(.data[[identity("x")]]))
  expect_false(quo_is_variable_reference(quo))
})
