#' @name when-any-all
NULL

#' @rdname when-any-all
#' @export
when_any <- function(..., missing = NULL, size = NULL) {
  check_dots_unnamed()
  list_pany(
    x = list2(...),
    missing = missing,
    size = size,
    x_arg = "",
    error_call = current_env()
  )
}

#' @rdname when-any-all
#' @export
when_all <- function(..., missing = NULL, size = NULL) {
  check_dots_unnamed()
  list_pall(
    x = list2(...),
    missing = missing,
    size = size,
    x_arg = "",
    error_call = current_env()
  )
}
