expect_hybrid <- function(data, expr, info = NULL, label = NULL) {
  expect_true(hybrid_impl(data, enquo(expr), rlang::caller_env()), info = info, label = label)
}

expect_not_hybrid <- function(data, expr, info = NULL, label = NULL) {
  expect_false(hybrid_impl(data, enquo(expr), rlang::caller_env()), info = info, label = label)
}
