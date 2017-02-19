check_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  check_hybrid_result_(lazyeval::f_capture(expr), ..., expected = expected, test_eval = test_eval)
}

check_hybrid_result_ <- function(expr, ..., expected, test_eval) {
  expect_error(expect_identical(with_hybrid_(expr, ...), expected), NA)
  if (test_eval) {
    expect_identical(eval_dots_(expr, ...), expected)
  }
}

check_not_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  check_not_hybrid_result_(lazyeval::f_capture(expr), ..., expected = expected, test_eval = test_eval)
}

check_not_hybrid_result_ <- function(expr, ..., expected, test_eval) {
  expect_error(expect_identical(without_hybrid_(expr, ...), expected), NA)
  if (test_eval) {
    expect_identical(eval_dots_(expr, ...), expected)
  }
}

expect_hybrid_error <- function(expr, ..., error) {
  expect_hybrid_error_(lazyeval::f_capture(expr), ..., error = error)
}

expect_hybrid_error_ <- function(expr, ..., error) {
  expect_error(
    with_hybrid_(expr, ...),
    error)
}

expect_not_hybrid_error <- function(expr, ..., error) {
  expect_not_hybrid_error_(lazyeval::f_capture(expr), ..., error = error)
}

expect_not_hybrid_error_ <- function(expr, ..., error) {
  expect_error(
    without_hybrid_(expr, ...),
    error)
}

expect_environments_empty <- function(x) {
  if (!is.environment(x)) x <- environment(x)
  if (isNamespace(x)) return()

  expect_equal(ls(x, all.names = TRUE), character())
  expect_environments_empty(parent.env(x))
}
