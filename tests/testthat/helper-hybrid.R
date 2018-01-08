expect_predicate <- function(actual, expected, label) {
  if (is.function(expected)) {
    expect_true((!! expected)(actual), label = label)
  } else {
    expect_identical(actual, !! expected, label = label)
  }
}

check_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  expr <- enquo(expr)
  expect_predicate(with_hybrid(!! expr, ...), expected, label = rlang::quo_label(expr))
  if (test_eval) {
    expect_predicate(eval_dots(!! expr, ...), expected, label = rlang::quo_label(expr))
  }
}

check_not_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  expr <- enquo(expr)
  expect_predicate(without_hybrid(!! expr, ...), expected, label = rlang::quo_label(expr))
  if (test_eval) {
    expect_predicate(eval_dots(!! expr, ...), expected, label = rlang::quo_label(expr))
  }
}

expect_hybrid_error <- function(expr, ..., error) {
  expr <- enquo(expr)
  expect_error(
    with_hybrid(!! expr, ...),
    error,
    label = rlang::quo_label(expr)
  )
}

expect_not_hybrid_error <- function(expr, ..., error) {
  expr <- enquo(expr)
  expect_error(
    without_hybrid(!! expr, ...),
    error,
    label = rlang::quo_label(expr)
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
