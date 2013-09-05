var_names <- function(vars) {
  nms <- names2(vars)
  unname(ifelse(nms == "", vars, nms))
}

#' Generate a basic SQL \code{SELECT} query.
#'
#' @param select a character vector of fields to select. Names are used to
#'   create \code{AS} aliases.
#' @param from a string giving the table name
#' @param from,where,group_by,having,order_by,limit,offset Select query 
#'   components. All inputs are \code{\link{escape}}d, so make sure they have
#'   been wrapped appropriately with \code{\link{sql}} or \code{\link{ident}}.
#' @export
#' @keywords internal
#' @family query construction
#' @return a \code{\link{query}} object
#' @examples
#' qry_select(lahman(), sql("*"), ident("mytable"))
#' qry_select(lahman(), sql("*"), ident("mytable"), sql("1 = 0"))
qry_select <- function(x, select, from, where = NULL, group_by = NULL,
                         having = NULL, order_by = NULL, limit = NULL,
                         offset = NULL, ...) {
  UseMethod("qry_select")
}
