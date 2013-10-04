#' Data manipulation for data frames.
#'
#' @param .data a data frame
#' @param ... variables interpreted in the context of \code{.data}
#' @param .env The environment in which to evaluate arguments not included
#'   in the data. The default should suffice for ordinary usage.
#' @examples
#' filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights, Year:DayOfWeek))
#' summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights, gained = ArrDelay - DepDelay))
#' head(arrange(hflights, Dest, desc(ArrDelay)))
#'
#' @name manip_df
NULL

#' @rdname manip_df
#' @export
#' @method filter data.frame
filter.data.frame <- function(.data, ..., env = parent.frame()) {
  conds <- dots(...)

  r <- vapply(conds, eval, env = .data, enclos = env,
    FUN.VALUE = logical(nrow(.data)))

  all <- rowSums(r, na.rm = TRUE) == ncol(r)
  .data[all, , drop = FALSE]
}

#' @S3method filter tbl_df
filter.tbl_df <- function(.data, ..., env = parent.frame()) {
  tbl_df(filter.data.frame(.data, ..., env = env))
}

#' @rdname manip_df
#' @export
#' @method summarise data.frame
summarise.data.frame <- function(.data, ...) {
  cols <- named_dots(...)
  data_env <- list2env(.data, parent = parent.frame())
  data_env$count <- function() nrow(.data)

  for (i in seq_along(cols)) {
    data_env[[names(cols)[i]]] <- eval(cols[[i]], data_env)
  }

  as_df(mget(unique(names(cols)), data_env))
}

#' @S3method summarise tbl_df
summarise.tbl_df <- function(.data, ...) {
  tbl_df(summarise.data.frame(.data, ...))
}


#' @rdname manip_df
#' @export
#' @method mutate data.frame
mutate.data.frame <- function(.data, ...) {
  cols <- named_dots(...)
  data_env <- list2env(.data, parent = parent.frame())

  for(i in seq_along(cols)) {
    data_env[[names(cols)[i]]] <- eval(cols[[i]], data_env)
  }

  out_cols <- union(names(.data), names(cols))
  as_df(mget(out_cols, data_env))
}

#' @S3method mutate tbl_df
mutate.tbl_df <- function(.data, ...) {
  tbl_df(mutate.data.frame(.data, ...))
}

#' @rdname manip_df
#' @export
#' @method arrange data.frame
arrange.data.frame <- function(.data, ...) {
  r <- eval(substitute(order(...)), .data, parent.frame())
  if(length(r) != nrow(.data)) {
    stop("Ordering vectors not the same length as data", call. = FALSE)
  }
  .data[r, , drop = FALSE]
}

#' @S3method arrange tbl_df
arrange.tbl_df <- function(.data, ...) {
  tbl_df(arrange.data.frame(.data, ...))
}

#' @rdname manip_df
#' @export
#' @method select data.frame
select.data.frame <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  input_vars <- vapply(input, as.character, character(1))
  
  .data[, input_vars, drop = FALSE]
}

#' @S3method select tbl_df
select.tbl_df <- function(.data, ...) {
  tbl_df(select.data.frame(.data, ...))
}

#' @S3method do data.frame
do.data.frame <- function(.data, .f, ...) {
  list(.f(.data, ...))
}

#' @S3method do tbl_df
do.tbl_df <- function(.data, .f, ...) {
  list(.f(.data, ...))
}
