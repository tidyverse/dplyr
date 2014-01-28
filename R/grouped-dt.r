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
#' if (require("hflights")) {
#' hflights_dt <- tbl_dt(hflights)
#' group_size(group_by(hflights_dt, Year, Month, DayofMonth))
#' group_size(group_by(hflights_dt, Dest))
#'
#' monthly <- group_by(hflights_dt, Month)
#' summarise(monthly, n = n(), delay = mean(ArrDelay))
#' }
grouped_dt <- function(data, vars) {
  stopifnot(is.data.table(data))
  if (length(vars) == 0) return(tbl_dt(data))

  is_name <- vapply(vars, is.name, logical(1))
  if (!all(is_name)) {
    stop("Data tables can only be grouped by variables, not expressions",
      call. = FALSE)
  }
  setkeyv(data, deparse_all(vars))

  structure(data, vars = vars, class = c("grouped_dt", "tbl_dt", "tbl", class(data)))
}

#' @export
groups.grouped_dt <- function(x) {
  attr(x, "vars")
}

#' @rdname grouped_dt
#' @param x an object to check
#' @export
is.grouped_dt <- function(x) inherits(x, "grouped_dt")

#' @export
print.grouped_dt <- function(x, ...) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("Groups: ", commas(deparse_all(groups(x))), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

#' @export
group_size.grouped_dt <- function(x) {
  summarise(x, n = n())$n
}

#' @export
regroup.data.table <- function(x, value) {
  grouped_dt(x, unname(value))
}

#' @export
regroup.tbl_dt <- function(x, value) {
  grouped_dt(x, unname(value))
}

#' @export
regroup.grouped_dt <- function(x, value) {
  grouped_dt(x, unname(value))
}

#' @export
ungroup.grouped_dt <- function(x) {
  attr(x, "vars") <- NULL
  class(x) <- setdiff(class(x), "grouped_dt")
  x
}
