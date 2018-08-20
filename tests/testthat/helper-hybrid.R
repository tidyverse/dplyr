expect_hybrid <- function(data, expr, info = NULL, label = NULL) {
  expr <- enquo(expr)
  expect_true(hybrid_call(data, !!expr), info = info, label = label)
}

expect_not_hybrid <- function(data, expr, info = NULL, label = NULL) {
  expr <- enquo(expr)
  expect_false(hybrid_call(data, !!expr), info = info, label = label)
}
