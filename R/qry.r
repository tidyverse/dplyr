var_names <- function(vars) {
  if (is.null(vars)) return(NULL)
  
  nms <- names2(vars)
  ident(unname(ifelse(nms == "", vars, nms)))
}

#' Generate a basic SQL \code{SELECT} query.
#'
#' @param select a character vector of fields to select. Names are used to
#'   create \code{AS} aliases.
#' @param from a string giving the table name
#' @param where,group_by,having,order_by,limit,offset Select query 
#'   components. All inputs are language objects
#' @export
#' @keywords internal
#' @family query construction
#' @return a \code{\link{query}} object
#' @examples
#' qry_select(src_lahman(), list(star()), ident("mytable"))
#' qry_select(src_lahman(), list(star()), ident("mytable"), list(quote(1 == 0)))
qry_select <- function(x, select, from, where = NULL, group_by = NULL,
                         having = NULL, order_by = NULL, limit = NULL,
                         offset = NULL, ...) {
  UseMethod("qry_select")
}
