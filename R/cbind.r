#' Efficiently cbind multiple data frames.
cbind_list <- function(...){
  cbind_all(list(...))
}
