expect_predicate <- function(actual, expected, label) {
  if (is.function(expected)) {
    expect_true((!!expected)(actual), label = label)
  } else {
    expect_identical(actual, !!expected, label = label)
  }
}

check_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  expr <- enquo(expr)
  expect_predicate(with_hybrid(!!expr, ...), expected, label = rlang::quo_label(expr))
  if (test_eval) {
    expect_predicate(eval_dots(!!expr, ...), expected, label = rlang::quo_label(expr))
  }
}

check_not_hybrid_result <- function(expr, ..., expected, test_eval = TRUE) {
  expr <- enquo(expr)
  expect_predicate(without_hybrid(!!expr, ...), expected, label = rlang::quo_label(expr))
  if (test_eval) {
    expect_predicate(eval_dots(!!expr, ...), expected, label = rlang::quo_label(expr))
  }
}

expect_hybrid_error <- function(expr, ..., error) {
  expr <- enquo(expr)
  expect_error(
    with_hybrid(!!expr, ...),
    error,
    label = rlang::quo_label(expr)
  )
}

expect_not_hybrid_error <- function(expr, ..., error) {
  expr <- enquo(expr)
  expect_error(
    without_hybrid(!!expr, ...),
    error,
    label = rlang::quo_label(expr)
  )
}

bad_hybrid_handler <- function(...) stop("Expected hybrid evaluation")
