expect_predicate <- function(actual, expected) {
  if (is.function(expected)) {
    expect_true(expected(actual))
  } else {
    expect_identical(actual, expected)
  }
}

check_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  check_hybrid_result_(rlang::enquo(expr), ..., expected = expected, test_eval = test_eval)
}

check_hybrid_result_ <- function(expr, ..., expected, test_eval) {
  expect_error(
    expect_predicate(with_hybrid_(expr, ...), expected), NA)
  if (test_eval) {
    expect_predicate(eval_dots_(expr, ...), expected)
  }
}

check_not_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  check_not_hybrid_result_(rlang::enquo(expr), ..., expected = expected, test_eval = test_eval)
}

check_not_hybrid_result_ <- function(expr, ..., expected, test_eval) {
  expect_error(
    expect_predicate(without_hybrid_(expr, ...), expected), NA)
  if (test_eval) {
    expect_predicate(eval_dots_(expr, ...), expected)
  }
}

expect_hybrid_error <- function(expr, ..., error) {
  expect_hybrid_error_(rlang::enquo(expr), ..., error = error)
}

expect_hybrid_error_ <- function(expr, ..., error) {
  expect_error(
    with_hybrid_(expr, ...),
    error
  )
}

expect_not_hybrid_error <- function(expr, ..., error) {
  expect_not_hybrid_error_(rlang::enquo(expr), ..., error = error)
}

expect_not_hybrid_error_ <- function(expr, ..., error) {
  expect_error(
    without_hybrid_(expr, ...),
    error
  )
}

expect_environments_clean <- function(x, stop_env = parent.frame()) {
  if (!is.environment(x)) x <- environment(x)
  if (identical(x, stop_env)) return()

  obj_names <- ls(x, all.names = TRUE)
  objs <- mget(obj_names, x)
  lapply(objs, expect_is, "environment")

  expect_environments_clean(parent.env(x), stop_env = stop_env)
}

bad_hybrid_handler <- function(...) stop("Expected hybrid evaluation")
