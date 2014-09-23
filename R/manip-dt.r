#' Data manipulation for data tables.
#'
#' @param .data a data table
#' @param ...,args variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @param .env The environment in which to evaluate arguments not included
#'   in the data. The default should suffice for ordinary usage.
#' @examples
#' if (require("data.table") && require("nycflights13")) {
#' # If you start with a data table, you end up with a data table
#' flights <- as.data.table(as.data.frame(flights))
#' filter(flights, month == 1, day == 1, dest == "DFW")
#' head(select(flights, year:day))
#' summarise(flights, delay = mean(arr_delay, na.rm = TRUE), n = length(arr_delay))
#' head(mutate(flights, gained = arr_delay - dep_delay))
#' head(arrange(flights, dest, desc(arr_delay)))
#'
#' # If you start with a tbl, you end up with a tbl
#' flights2 <- as.tbl(flights)
#' filter(flights2, month == 1, day == 1, dest == "DFW")
#' head(select(flights2, year:day))
#' summarise(flights2, delay = mean(arr_delay, na.rm = TRUE), n = length(arr_delay))
#' head(mutate(flights2, gained = arr_delay - dep_delay))
#' head(arrange(flights2, dest, desc(arr_delay)))
#' }
#' @name manip_dt
NULL

and_expr <- function(exprs) {
  assert_that(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @rdname manip_dt
#' @export
filter.data.table <- function(.data, ..., .env = parent.frame()) {
  expr <- and_expr(dots(...))
  call <- substitute(.data[expr, ])

  eval_env <- new.env(parent = .env)
  eval_env$.data <- .data

  eval(call, eval_env)
}

#' @export
filter.tbl_dt <- function(.data, ..., .env = parent.frame()) {
  tbl_dt(
    filter.data.table(.data, ..., .env = .env)
  )
}

#' @rdname manip_dt
#' @export
summarise_.data.table <- function(.data, dots) {
  dots <- lazyeval::as.lazy_dots(dots, parent.frame())

  list_call <- lazyeval::make_call(quote(list), dots)
  call <- substitute(.data[, vars], list(vars = list_call$expr))

  eval(call, parent.frame())
}

#' @export
summarise_.tbl_dt <- function(.data, dots) {
  dots <- lazyeval::as.lazy_dots(dots, parent.frame())
  tbl_dt(
    summarise_.data.table(.data, dots)
  )
}

#' @rdname manip_dt
#' @export
mutate.data.table <- function(.data, ..., inplace = FALSE) {
  if (!inplace) .data <- copy(.data)

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data

  cols <- named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(i in seq_along(cols)) {
    call <- substitute(data[, lhs := rhs],
      list(lhs = as.name(names(cols)[[i]]), rhs = cols[[i]]))
    eval(call, env)
  }

  .data
}

#' @export
mutate.tbl_dt <- function(.data, ...) {
  tbl_dt(
    mutate.data.table(.data, ...)
  )
}

#' @rdname manip_dt
#' @export
arrange.data.table <- function(.data, ...) {
  call <- substitute(data[order(...)])
  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data
  out <- eval(call, env)

  eval(call, env)
}

#' @export
arrange.tbl_dt <- function(.data, ...) {
  tbl_dt(
    arrange.data.table(.data, ...)
  )
}

#' @rdname manip_dt
#' @export
select_.data.table <- function(.data, args) {
  args <- lazyeval::as.lazy_dots(args, parent.frame())
  vars <- select_vars_(names(.data), args)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  setnames(out, names(vars))
  out
}

#' @rdname manip_dt
#' @export
rename.data.table <- function(.data, ...) {
  vars <- rename_vars_(names(.data), lazyeval::lazy_dots(...))

  out <- .data[, vars, drop = FALSE, with = FALSE]
  setnames(out, names(vars))
  out
}

#' @export
select_.tbl_dt <- function(.data, args) {
  args <- lazyeval::as.lazy_dots(args, parent.frame())
  tbl_dt(
    select_.data.table(.data, args)
  )
}

#' @export
do.data.table <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @export
do.tbl_dt <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}
