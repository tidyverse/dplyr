#' @export
arrange_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  arrange_impl(.data, dots)
}

#' @export
filter_.tbl_df    <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  filter_impl(.data, dots)
}

#' @export
slice_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  slice_impl(.data, dots)
}

#' @export
mutate_.tbl_df  <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  mutate_impl(.data, dots)
}

#' @export
summarise_.tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  summarise_impl(.data, dots)
}


#' @export
select_.grouped_df <- function(.data, args) {
  args <- lazyeval::as.lazy_dots(args, parent.frame())
  vars <- select_vars_(names(.data), args,
    include = as.character(groups(.data)))

  select_impl(.data, vars)
}

#' @export
rename.grouped_df <- function(.data, ...) {
  vars <- rename_vars_(names(.data), lazyeval::lazy_dots(...))

  select_impl(.data, vars)
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

