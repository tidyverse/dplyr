#' Copy a local data frame to a remote src
#'
#' This function uploads a local data frame into a remote data source, creating
#' the table definition as needed. Wherever possible, the new object will be
#' temporary, limited to the current connection to the source.
#'
#' @param dest remote data source
#' @param df local data frame
#' @param name name for new remote table.
#' @param overwrite If `TRUE`, will overwrite an existing table with
#'   name `name`. If `FALSE`, will throw an error if `name` already
#'   exists.
#' @param ... other parameters passed to methods.
#' @seealso [collect()] for the opposite action; downloading remote data into
#'   a local dbl.
#' @return a `tbl` object in the remote source
#' @export
#' @examples
#' \dontrun{
#' iris2 <- dbplyr::src_memdb() %>% copy_to(iris, overwrite = TRUE)
#' iris2
#' }
copy_to <- function(dest, df, name = deparse(substitute(df)),
                    overwrite = FALSE, ...) {
  UseMethod("copy_to")
}

#' Copy tables to same source, if necessary
#'
#' @param x,y `y` will be copied to `x`, if necessary.
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into the
#'   same src as `x`.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... Other arguments passed on to methods.
#' @export
auto_copy <- function(x, y, copy = FALSE, ...) {
  if (same_src(x, y)) return(y)

  if (!copy) {
    glubort(NULL, "`x` and `y` must share the same src, ",
      "set `copy` = TRUE (may be slow)."
    )
  }

  UseMethod("auto_copy")
}

#' @export
auto_copy.data.frame <- function(x, y, copy = FALSE, ...) {
  as.data.frame(y)
}
