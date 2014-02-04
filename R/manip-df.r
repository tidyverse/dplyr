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
arrange.tbl_df    <- .data_dots(arrange_impl)

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
  vars <- select_vars(names(.data), ..., env = parent.frame())

  # Don't remove grouping vars!
  missing <- setdiff(as.character(groups(.data)), vars)
  if (length(missing) > 0) {
    stop("selection doesn't include grouping variables: ",
      paste0(missing, collapse = ","), call. = FALSE)
  }

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

#' @export
do.grouped_df <- function(.data, .f, ...) {
  if (is.null(attr(.data, "indices"))) {
    .data <- grouped_df_impl(.data, attr(.data, "vars"), attr(.data, "drop"))
  }

  index <- attr(.data, "indices")
  out <- vector("list", length(index))

  for (i in seq_along(index)) {
    subs <- .data[index[[i]] + 1L, , drop = FALSE]
    out[[i]] <- .f(subs, ...)
  }

  out
}
