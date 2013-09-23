#' Join two tbls together
#' 
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of \code{x}.
#' 
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with 
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right. 
#' @param type a string giving the join type. Possible values are left (the 
#'   default), right, full and inner. Not all types will be supported by all
#'   tbls.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the 
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... other parameters passed onto individual methods
#' @export
join <- function(x, y, by = NULL, type = "left", copy = FALSE, ...) {
  UseMethod("join")
}

#' Semi joins and anti joins.
#' 
#' Groups are ignored for the purpose of joining, but the result preserves
#' the grouping of \code{x}.
#' 
#' A semi join keeps all records from \code{x} that have matching rows in 
#' \code{y}. An anti-join does the opposite: it preserves all records from 
#' \code{x} that don't have matching values in \code{y}. Both keep the columns 
#' of \code{x}, and don't include any from \code{y}.
#' 
#' A semi join differs from an inner join (with just the \code{by} variables)
#' because an inner join will return one row of \code{x} for each matching row
#' of \code{y}, where a semi join will never duplicated rows in \code{x}.
#' 
#' @inheritParams join
#' @param anti If \code{TRUE}, performs an anti join instead of a semi join.
#' @export
semi_join <- function(x, y, by = NULL, anti = FALSE, copy = FALSE, ...) {
  UseMethod("semi_join")
}


common_by <- function(x, y) {
  by <- intersect(tbl_vars(x), tbl_vars(y))
  message("Joining by: ", capture.output(dput(by)))
  by
}