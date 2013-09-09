#' A grouped data table.
#'
#' The easiest way to create a grouped data table is to call the \code{group_by}
#' method on a data table or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @export
#' @examples
#' hflights_dt <- data.table(hflights)
#' group_size(group_by(hflights_dt, Year, Month, DayofMonth))
#' group_size(group_by(hflights_dt, Dest))
grouped_dt <- function(data, vars) {
  stopifnot(is.data.table(data))
  if (length(vars) == 0) return(tbl_dt(data))
  
  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  setkeyv(data, deparse_all(vars))

  data <- list(obj = data, vars = vars)
  structure(data, class = c("grouped_dt", "tbl_dt", "tbl"))
}

#' @S3method groups grouped_dt
groups.grouped_dt <- function(x) {
  x$vars
}

#' @rdname grouped_dt
#' @param x an object to check
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @S3method print grouped_dt
print.grouped_dt <- function(x, ...) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x$obj)
}

#' @S3method group_size grouped_dt
group_size.grouped_dt <- function(x) {
  summarise(x, n = .N)$obj$n
}

#' @method group_by data.table
#' @export
#' @rdname grouped_dt
#' @param ... variables to group by
group_by.data.table <- function(x, ...) {
  vars <- dots(...)
  grouped_dt(x, c(groups(x), vars))
}

#' @method group_by tbl_dt
#' @export
#' @rdname grouped_dt
group_by.tbl_dt <- function(x, ...) {
  vars <- dots(...)
  grouped_dt(x, c(groups(x), vars))
}

#' @S3method group_by grouped_dt
group_by.grouped_dt <- function(x, ...) {
  vars <- dots(...)
  grouped_dt(x$obj, c(groups(x), vars))
}

#' @S3method ungroup grouped_dt
ungroup.grouped_dt <- function(x) {
  tbl_dt(x$obj)
}

