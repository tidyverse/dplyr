expect_no_error <- function(object, ...) {
  expect_error({{ object }}, NA, ...)
}
expect_no_warning <- function(object, ...) {
  expect_warning({{ object }}, NA, ...)
}

sig_caller_env <- function() {
  signal(
    "",
    "dplyr:::test_caller_env",
    out = peek_mask()$get_caller_env()
  )
}
expect_caller_env <- function(expr) {
  env <- catch_cnd(expr, "dplyr:::test_caller_env")$out
  expect_equal(env, caller_env())
}
