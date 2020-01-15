#' Combine vectors
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' `combine()` is deprecated in favour of [vctrs::vec_c()]. `combine()`
#' attempted to automatically guess whether you wanted [c()] or [unlist()],
#' but could fail in surprising ways. We now believe it's better to be explicit.
#'
#' @param ... Vectors to combine.
#' @keywords internal
#' @export
#' @examples
#' f1 <- factor("a")
#' f2 <- factor("b")
#'
#' combine(f1, f2)
#' # ->
#' vctrs::vec_c(f1, f1)
#'
#' combine(list(f1, f2))
#' # ->
#' vctrs::vec_c(!!!list(f1, f2))
combine <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "combine()", "vctrs::vec_c()")

  args <- list2(...)
  if (length(args) == 1 && is.list(args[[1]])) {
    args <- args[[1]]
  }
  args <- keep(args, function(.x) !is.null(.x))
  names(args) <- NULL
  if (length(args) == 0) {
    logical()
  } else {
    vec_c(!!!args)
  }
}
