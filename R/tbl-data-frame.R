# These could potentially be rewritten to avoid any copies, but since this
# is just a convenience layer, I didn't bother. They should still be fast.

#' @export
filter.data.frame <- function(.data, ...) {
  as.data.frame(filter(tbl_df(.data), ...))  
}
#' @export
summarise.data.frame <- function(.data, ...) {
  as.data.frame(summarise(tbl_df(.data), ...))  
}
#' @export
mutate.data.frame <- function(.data, ...) {
  as.data.frame(mutate(tbl_df(.data), ...))  
}
#' @export
arrange.data.frame <- function(.data, ...) {
  as.data.frame(arrange(tbl_df(.data), ...))  
}
#' @export
select.data.frame <- function(.data, ...) {
  as.data.frame(select(tbl_df(.data), ...))  
}
#' @export
do.data.frame <- function(.data, .f, ...) {
  list(.f(.data, ...))
}
