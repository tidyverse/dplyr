#' @name when-any-all
NULL

#' @rdname when-any-all
#' @export
when_any <- function(..., na_rm = FALSE, size = NULL) {
  check_dots_unnamed()

  check_bool(na_rm)
  missing <- if (na_rm) FALSE else NULL

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
when_all <- function(..., na_rm = FALSE, size = NULL) {
  check_dots_unnamed()

  check_bool(na_rm)
  missing <- if (na_rm) TRUE else NULL

  list_pall(
    x = list2(...),
    missing = missing,
    size = size,
    x_arg = "",
    error_call = current_env()
  )
}
