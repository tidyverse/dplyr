#' Data manipulation for data source objects.
#'
#' All data manipulation on data source objects is lazy: all functions return a
#' lightweight object representing the action. This is only turned into an
#' result when \code{\link{render}} is called.
#'
#' @examples
#' data("baseball", package = "plyr")
#' bdf <- data_frame_source(baseball)
#' filter(bdf, year > 1980)
#' summarise(bdf, g = mean(g))
#' mutate(bdf, cyear = year - min(year) + 1)
#' arrange(bdf, id, desc(year))
#' @name manip_source
NULL

#' @rdname manip_source
#' @export
#' @method filter source
filter.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)

  `_data`$filter <- c(`_data`$filter, args)
  `_data`
}

#' @rdname manip_source
#' @export
#' @method summarise source
summarise.source <- function(`_data`, ..., `_env` = parent.frame()) {
  if (!is.null(`_data`$mutate)) {
    stop("Only one of summarise and mutate can be used",
      call. = FALSE)
  }

  args <- lapply(named_dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$summarise <- c(`_data`$summarise, args)
  `_data`
}

#' @rdname manip_source
#' @export
#' @method mutate source
mutate.source <- function(`_data`, ..., `_env` = parent.frame()) {
  if (!is.null(`_data`$summarise)) {
    stop("Only one of summarise and mutate can be used",
      call. = FALSE)
  }
  args <- lapply(named_dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$mutate <- c(`_data`$mutate, args)
  `_data`
}

#' @rdname manip_source
#' @export
#' @method arrange source
arrange.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$arrange <- c(`_data`$arrange, args)
  `_data`
}

#' @rdname manip_source
#' @export
#' @method select source
select.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)

  `_data`$select <- c(`_data`$select, args)
  `_data`
}

#' @rdname manip_source
#' @export
#' @method limit source
limit.source <- function(`_data`, n) {
  `_data`$limit <- n
  `_data`
}
