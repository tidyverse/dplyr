#' Defunct functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions were deprecated for at least two years before being
#' made defunct. If there's a known replacement, calling the function
#' will tell you about it.
#'
#' @keywords internal
#' @name defunct
NULL

#' @usage # Deprecated in 1.0.0 -------------------------------------
#' @name defunct
NULL

#' @export
#' @rdname defunct
combine <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "combine()", "vctrs::vec_c()")
}

#' @export
#' @rdname defunct
src_mysql <- function(
  dbname,
  host = NULL,
  port = 0L,
  username = "root",
  password = "",
  ...
) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "src_mysql()",
    details = "Please use `tbl()` directly with a database connection"
  )
}

#' @export
#' @rdname defunct
src_postgres <- function(
  dbname = NULL,
  host = NULL,
  port = NULL,
  user = NULL,
  password = NULL,
  ...
) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "src_postgres()",
    details = "Please use `tbl()` directly with a database connection"
  )
}

#' @export
#' @rdname defunct
src_sqlite <- function(path, create = FALSE) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "src_sqlite()",
    details = "Please use `tbl()` directly with a database connection"
  )
}

#' @export
#' @rdname defunct
src_local <- function(tbl, pkg = NULL, env = NULL) {
  lifecycle::deprecate_stop("1.0.0", "src_local()")
}

#' @export
#' @rdname defunct
src_df <- function(pkg = NULL, env = NULL) {
  lifecycle::deprecate_stop("1.0.0", "src_df()")
}

#' @export
#' @rdname defunct
tbl_df <- function(data) {
  lifecycle::deprecate_stop("1.0.0", "tbl_df()", "tibble::as_tibble()")
}

#' @export
#' @rdname defunct
as.tbl <- function(x, ...) {
  lifecycle::deprecate_stop("1.0.0", "as.tbl()", "tibble::as_tibble()")
}

#' @export
#' @rdname defunct
add_rownames <- function(df, var = "rowname") {
  lifecycle::deprecate_stop(
    "1.0.0",
    "add_rownames()",
    "tibble::rownames_to_column()"
  )
}
