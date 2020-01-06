#' Convert row names to an explicit variable.
#'
#' Deprecated, use [tibble::rownames_to_column()] instead.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @keywords internal
#' @export
#' @examples
#' mtcars %>% tbl_df()
#'
#' mtcars %>% add_rownames()
add_rownames <- function(df, var = "rowname") {
  warning(
    "Deprecated, use tibble::rownames_to_column() instead.",
    call. = FALSE
  )

  stopifnot(is.data.frame(df))

  rn <- as_tibble(setNames(list(rownames(df)), var))
  rownames(df) <- NULL

  bind_cols(rn, df)
}
