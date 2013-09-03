#' Create an SQL tbl (abstract)
#'
#' This method shouldn't be called be users - it should only be used by
#' backend implementors who are creating backends that extend the basic
#' sql behaviour.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... other fields needed by the subclass
#' @param select,filter,arrange default sql components.
tbl_sql <- function(subclass, ..., select = NULL, filter = NULL,
                       arrange = NULL) {
  tbl(c(subclass, "tbl_sql"), ..., select = select, filter = filter,
    arrange = arrange)
}
