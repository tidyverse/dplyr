.data_dots <- function(fun, DOTS = dots){
  f <- function(.data, ...){}
  body(f) <- substitute({
    FUN(.data, DOTS(...), environment() )
  }, list( FUN = substitute(fun), DOTS = substitute(DOTS)))
  attr(f, "srcref") <- NULL
  f
}

#' Data manipulation for data frames.
#'
#' @param .data a data frame
#' @param ... variables interpreted in the context of \code{.data}
#' @examples
#' if (require("hflights")) {
#' filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights, Year:DayOfWeek))
#' summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights, gained = ArrDelay - DepDelay))
#' head(arrange(hflights, Dest, desc(ArrDelay)))
#' }
#' @name manip_df
NULL

#' @rdname manip_df
#' @export
arrange.tbl_df    <- function(.data, ...){
  arrange_impl(.data, environment())  
}

#' @rdname manip_df
#' @export
filter.tbl_df    <- .data_dots(filter_impl)

integer_filter   <- .data_dots(integer_filter_impl)

#' @rdname manip_df
#' @export
mutate.tbl_df    <- .data_dots(mutate_impl, named_dots)

#' @rdname manip_df
#' @export
summarise.tbl_df <- .data_dots(summarise_impl, named_dots)

#' @rdname manip_df
#' @export
select.tbl_df <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame())
  select_impl(.data, vars)
}

#' @export
select.grouped_df <- function(.data, ...) {
  vars <- select_vars(names(.data), ..., env = parent.frame(),
    include = as.character(groups(.data)))

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

