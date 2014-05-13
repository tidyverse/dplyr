#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the \code{group_by}
#' method on a data frame or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param drop if \code{TRUE} preserve all factor levels, even those without
#'   data.
#' @export
grouped_df <- function(data, vars, drop = TRUE) {
  if (length(vars) == 0) {
    return(tbl_df(data))
  }

  assert_that(is.data.frame(data), is.list(vars), is.flag(drop))
  grouped_df_impl(data, unname(vars), drop)
}

#' @rdname grouped_df
#' @export
is.grouped_df <- function(x) inherits(x, "grouped_df")

#' @export
print.grouped_df <- function(x, ..., n=NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n=n)
}

#' @export
group_size.grouped_df <- function(x) {
  group_size_grouped_cpp(x)
}

#' @export
groups.grouped_df <- function(x) {
  attr(x, "vars")
}

#' @export
as.data.frame.grouped_df <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  x <- ungroup(x)
  class(x) <- "data.frame"
  x
}

#' @export
ungroup.grouped_df <- function(x) {
  ungroup_grouped_df(x)
}
