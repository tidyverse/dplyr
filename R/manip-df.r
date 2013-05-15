#' Data manipulation for data frames.
#'
#' @param .data a data frame
#' @param ... variables interpreted in the context of \code{.data}
#' @examples
#' data("baseball", package = "plyr")
#
#' filter(baseball, year > 2005, g > 130)
#' head(select(baseball, id:team))
#' summarise(baseball, g = mean(g), n = count())
#' head(mutate(baseball, rbi = 1.0 * r / ab))
#' head(arrange(baseball, id, desc(year)))
#'
#' @name manip_data_frame
NULL

#' @rdname manip_data_frame
#' @export
#' @method filter data.frame
filter.data.frame <- function(.data, ...) {
  conds <- dots(...)

  r <- vapply(conds, eval, env = .data, enclos = parent.frame(),
    FUN.VALUE = logical(nrow(.data)))

  all <- rowSums(r, na.rm = TRUE) == ncol(r)
  .data[all, , drop = FALSE]
}

#' @S3method filter source_df
filter.source_df <- function(.data, ...) {
  source_df(
    filter.data.frame(.data$obj, ...)
  )
}

#' @rdname manip_data_frame
#' @export
#' @method summarise data.frame
summarise.data.frame <- function(.data, ...) {
  cols <- named_dots(...)
  data_env <- list2env(.data, parent = parent.frame())
  data_env$count <- function() nrow(.data)

  for (col in names(cols)) {
    data_env[[col]] <- eval(cols[[col]], data_env)
  }

  as_df(mget(names(cols), data_env))
}

#' @S3method summarise source_df
summarise.source_df <- function(.data, ...) {
  source_df(
    summarise.data.frame(.data$obj, ...)
  )
}


#' @rdname manip_data_frame
#' @export
#' @method mutate data.frame
mutate.data.frame <- function(.data, ...) {
  cols <- named_dots(...)
  data_env <- list2env(.data, parent = parent.frame())

  for(col in names(cols)) {
    data_env[[col]] <- eval(cols[[col]], data_env)
  }

  out_cols <- union(names(.data), names(cols))
  as_df(mget(out_cols, data_env))
}

#' @S3method mutate source_df
mutate.source_df <- function(.data, ...) {
  source_df(
    mutate.data.frame(.data$obj, ...)
  )
}

#' @rdname manip_data_frame
#' @export
#' @method arrange data.frame
arrange.data.frame <- function(.data, ...) {
  r <- eval(substitute(order(...)), .data, parent.frame())
  if(length(r) != nrow(.data)) {
    stop("Ordering vectors not the same length as data", call. = FALSE)
  }
  .data[r, , drop = FALSE]
}

#' @S3method arrange source_df
arrange.source_df <- function(.data, ...) {
  source_df(
    arrange.data.frame(.data$obj, ...)
  )
}

#' @rdname manip_data_frame
#' @export
#' @method select data.frame
select.data.frame <- function(.data, ...) {
  input <- var_eval(.data, dots(...), parent.frame())
  .data[, input, drop = FALSE]
}

#' @S3method select source_df
select.source_df <- function(.data, ...) {
  source_df(
    select.data.frame(.data$obj, ...)
  )
}
