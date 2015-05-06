#' Copy a local data frame to a remote src.
#'
#' This uploads a local data frame into a remote data source, creating the
#' table definition as needed. Wherever possible, the new object will be
#' temporary, limited to the current connection to the source.
#'
#' @param dest remote data source
#' @param df local data frame
#' @param name name for new remote table.
#' @param ... other parameters passed to methods.
#' @return a \code{tbl} object in the remote source
#' @export
copy_to <- function(dest, df, name = deparse(substitute(df)), ...) {
  UseMethod("copy_to")
}

auto_copy <- function(x, y, copy = FALSE, ...) {
  if (same_src(x, y)) return(y)

  if (!copy) {
    stop("x and y don't share the same src. Set copy = TRUE to copy y into ",
      "x's source (this may be time consuming).", call. = FALSE)
  }

  UseMethod("auto_copy")
}
