#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the \code{group_by}
#' method on a data frame or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param lazy if \code{TRUE}, index will be computed lazily every time it
#'   is needed. If \code{FALSE}, index will be computed up front on object
#'   creation.
#' @param drop if \code{TRUE} preserve all factor levels, even those without
#'   data.
grouped_df <- function(data, vars, lazy = TRUE, drop = TRUE) {
  if (length(vars) == 0) {
    return(tbl_df(data))
  }

  assert_that(is.data.frame(data), is.list(vars), is.flag(lazy), is.flag(drop))
  
  attr(data, "vars") <- vars
  attr(data, "drop") <- drop

  if (!lazy) {
    data <- build_index_cpp(data)
  }
  class(data) <- c("grouped_cpp", "tbl_cpp", "tbl", "data.frame")
  data
}

#' @rdname grouped_df
#' @export
is.grouped_df <- function(x) inherits(x, "grouped_cpp")

#' @export
print.grouped_cpp <- function(x, ...) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @export
group_size.grouped_cpp <- function(x) {
  group_size_grouped_cpp(x)
}

#' @export
groups.grouped_cpp <- function(x, value) {
  attr(x, "vars")
}

#' @export
as.data.frame.grouped_cpp <- function(x, row.names = NULL,
                                            optional = FALSE, ...) {
  x <- ungroup(x)
  class(x) <- "data.frame"
  x
}

#' @export
ungroup.grouped_cpp <- function(x) {
  attr(x, "vars") <- NULL
  attr(x, "index") <- NULL
  attr(x, "labels") <- NULL
  attr(x, "drop") <- NULL
  
  class(x) <- c("tbl_cpp", "tbl", "data.frame")
  x
}
