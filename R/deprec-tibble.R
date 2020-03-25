#' Coerce to a tibble
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [tibble::as_tibble()] instead.
#'
#' @export
#' @keywords internal
#' @param data,x Object to coerce
tbl_df <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "tbl_df()", "tibble::as_tibble()")
  # Works in tibble < 1.5.0 too, because .name_repair will be
  # swallowed by the ellipsis
  as_tibble(data, .name_repair = "check_unique")
}

#' @export
#' @rdname tbl_df
as.tbl <- function(x, ...) {
  lifecycle::deprecate_warn("1.0.0", "as.tbl()", "tibble::as_tibble()")
  UseMethod("as.tbl")
}

#' @export
as.tbl.tbl <- function(x, ...) x

#' @export
as.tbl.data.frame <- function(x, ...) {
  as_tibble(x)
}


#' Convert row names to an explicit variable.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [tibble::rownames_to_column()] instead.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @keywords internal
#' @export
add_rownames <- function(df, var = "rowname") {
  lifecycle::deprecate_warn("1.0.0", "add_rownames()", "tibble::rownames_to_column()")

  abort_if_not(is.data.frame(df))

  rn <- as_tibble(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}
