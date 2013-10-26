.data_dots <- function(fun, DOTS = dots){
  f <- function(.data, ...){}
  body(f) <- substitute({
    parent_frame <- parent.frame()
    env <- as.environment(.data)
    parent.env(env) <- parent_frame
    FUN(.data, DOTS(...) , env )   
  }, list( FUN = substitute(fun), DOTS = substitute(DOTS)))
  attr(f, "srcref") <- NULL
  f
}

#' @S3method arrange tbl_cpp
arrange.tbl_cpp    <- .data_dots(arrange_impl)

#' @S3method filter tbl_cpp
filter.tbl_cpp    <- .data_dots(filter_impl)

#' @S3method mutate tbl_cpp
mutate.tbl_cpp    <- .data_dots(mutate_impl, named_dots)

#' @S3method summarise tbl_cpp
summarise.tbl_cpp <- .data_dots(summarise_impl, named_dots)

#' @S3method select tbl_cpp
select.tbl_cpp <- function(.data, ...) {
  tbl_cpp(select.data.frame(.data, ...))
}

# Other methods that currently don't have a better home -----------------------

order_ <- function(..., data){
  parent_frame <- parent.frame()
  if(missing(data)) {
    env <- parent_frame
  } else {
    env <- as.environment(data)
    parent.env(env) <- parent_frame
  }
  order_impl(dots(...) , env)
}  

equal_ <- function(x, y){
  equal_data_frame(x, y) 
}

all_equal_ <- function(...){
  env <- parent.frame()
  all_equal_data_frame(dots(...), env)     
}

sort_ <- function(data){
  sort_impl(data)
}
