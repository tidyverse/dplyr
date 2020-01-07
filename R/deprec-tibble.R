#' Create a data frame tbl.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [tibble::as_tibble()] instead.
#'
#' @export
#' @keywords internal
#' @param data a data frame
tbl_df <- function(data) {
  lifecycle::deprecate_warn("1.0.0", "tbl_df()", "tibble::as_tibble()")
  # Works in tibble < 1.5.0 too, because .name_repair will be
  # swallowed by the ellipsis
  as_tibble(data, .name_repair = "check_unique")
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
  lifecycle::deprecate_warn("1.0.0", "tbl_df()", "tibble::rownames_to_column()")

  stopifnot(is.data.frame(df))

  rn <- as_tibble(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}
