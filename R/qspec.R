#' Query specifications
#'
#' TBD.
qspec <- function(name, x, ...) {
  structure(
    x,
    ...,
    classes = c(paste0("qspec_", name), "qspec"))
}

#' @export
as.qspec_select <- function(x, ..., .data) UseMethod("as.qspec_select")

#' @export
as.qspec_select.character <- function(x, ..., .data) {
  if (length(list(...)) > 0L) {
    stop("Cannot use both .dots as character and ... in select_()",
         call. = FALSE)
  }

  if (!all(x %in% tbl_vars(.data))) {
    stop("Columns ", paste0("'", setdiff(x, names(.data)), "'"),
         " not found.", call. = FALSE)
  }

  missing_names <- names2(x) == ""
  names(x)[missing_names] <- x[missing_names]

  x <- ensure_grouped_vars(x, .data)
  qspec("select", x)
}

#' @export
as.qspec_select.integer <- function(x, ..., .data) {
  if (length(list(...)) > 0L) {
    stop("Cannot use both .dots as numeric and ... in select_()",
         call. = FALSE)
  }

  as.qspec_select.NULL(NULL, lazyeval::lazy(x), .data = .data)
}

#' @export
as.qspec_select.numeric <- as.qspec_select.integer

#' @export
as.qspec_select.lazy_dots <- function(x, ..., .data) {
  dots <- lazyeval::all_dots(x, ...)
  vars <- select_vars_(tbl_vars(.data), dots)
  as.qspec_select.character(vars, .data = .data)
}

#' @export
as.qspec_select.NULL <- as.qspec_select.lazy_dots

#' @export
as.qspec_select.list <- as.qspec_select.lazy_dots
