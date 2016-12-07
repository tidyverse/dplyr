expect_hybrid <- function(expr, ..., expected, test_eval = TRUE) {
  expect_hybrid_(lazyeval::f_capture(expr), ..., expected = expected, test_eval = test_eval)
}

expect_hybrid_ <- function(expr, ..., expected, test_eval) {
  expect_identical(with_hybrid_(expr, ...), expected)
  if (test_eval) {
    expect_identical(eval_dots_(expr, ...), expected)
  }
}

expect_not_hybrid <- function(expr, ..., expected, test_eval = TRUE) {
  expect_not_hybrid_(lazyeval::f_capture(expr), ..., expected = expected, test_eval = test_eval)
}

expect_not_hybrid_ <- function(expr, ..., expected, test_eval) {
  expect_identical(without_hybrid_(expr, ...), expected)
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
