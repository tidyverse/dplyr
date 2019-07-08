capture_error_msg <- function(expr) {
  cat(rlang::catch_cnd(expr, classes = "error")$message, "\n", sep="")
}
