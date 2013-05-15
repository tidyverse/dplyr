#' A grouped data table.
#'
#' The easiest way to create a grouped data table is to call the \code{group_by}
#' method on a data table or data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a data source or data frame.
#' @param vars a list of quoted variables.
grouped_dt <- function(data, vars) {
  stopifnot(is.data.table(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  setkeyv(data, deparse_all(vars))

  data <- list(obj = data, vars = vars)
  structure(data, class = c("grouped_dt", "source_dt", "source"))
}

#' @rdname grouped_dt
#' @param x an object to check
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @S3method print grouped_dt
print.grouped_dt <- function(x, ...) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(x$vars)), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @method group_by data.table
#' @export
#' @rdname grouped_dt
#' @param ... variables to group by
group_by.data.table <- function(x, ...) {
  vars <- dots(...)

  grouped_dt(x, vars)
}

#' @method group_by source_dt
#' @export
#' @rdname grouped_dt
group_by.source_dt <- function(x, ...) {
  vars <- dots(...)
  grouped_dt(x$obj, vars)
}

#' @S3method ungroup grouped_dt
ungroup.grouped_dt <- function(x) {
  source_dt(x$obj)
}

