#' @S3method inner_join tbl_cpp
inner_join.tbl_cpp <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  inner_join_impl(x,y,by)
}

#' @S3method left_join tbl_cpp
left_join.tbl_cpp <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  left_join_impl(x,y,by)
}

#' @S3method semi_join tbl_cpp
semi_join.tbl_cpp <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  semi_join_impl(x,y,by)
}

#' @S3method anti_join tbl_cpp
anti_join.tbl_cpp <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  anti_join_impl(x,y,by)
}
