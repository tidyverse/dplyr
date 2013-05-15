#' Create a data table source.
#'
#' A data table source wraps a local data table.
#'
#' @export
#' @param data a data table
#' @param name the name of the data frame: used to help you remember where it
#'   came from. If not supplied, taken from the deparsed expression passed
#'   to the \code{data} argument.
#' @examples
#' ds <- data_table_source(mtcars)
#' ds
#' as.data.frame(ds)
#' as.source(mtcars)
data_table_source <- function(data, name = NULL) {
  if (!require("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }

  name <- name %||% deparse(substitute(data))
  assert_that(is.string(name))

  # Hack until new version of data.table comes out
  assignInNamespace("cedta", function(...) TRUE, "data.table")

  data <- as.data.table(data)
  structure(list(obj = data, name = name),
    class = c("source_data_table", "source"))
}

#' @S3method as.source data.table
as.source.data.table <- function(x, name = NULL, ...) {
  name <- name %||% deparse(substitute(x))
  data_table_source(x, name = name)
}

#' @S3method source_name source_data_table
source_name.source_data_table <- function(x) x$name

#' @S3method source_vars source_data_table
source_vars.source_data_table <- function(x) copy(names(x$obj))

# Standard data frame methods --------------------------------------------------

#' Coerce data table to source.
#'
#' @export
#' @keywords internal
as.data.table.source_data_table <- function(x, keep.rownames = NULL) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)

  x$obj
}

#' @S3method as.data.frame source_data_table
as.data.frame.source_data_table <- function(x, row.names = NULL,
                                            optional = NULL, ...) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  as.data.frame(x$obj)
}

#' @S3method print source_data_table
print.source_data_table <- function(x, ...) {
  cat("Source:     local object\n", sep = "")
  cat("Data table: ", dQuote(x$name), dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_data_table
dimnames.source_data_table <- function(x) copy(dimnames(x$obj))

#' @S3method dim source_data_table
dim.source_data_table <- function(x) dim(x$obj)

#' @S3method head source_data_table
head.source_data_table <- function(x, ...) as.data.frame(head(x$obj, ...))

#' @S3method tail source_data_table
tail.source_data_table <- function(x, ...) as.data.frame(tail(x$obj, ...))
