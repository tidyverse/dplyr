#' Data manipulation for data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @param .env The environment in which to evaluate arguments not included
#'   in the data. The default should suffice for ordinary usage.
#' @examples
#' if (require("data.table")) {
#' # If you start with a data table, you end up with a data table
#' hflights <- as.data.table(hflights)
#' filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights, Year:DayOfWeek))
#' summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights, gained = ArrDelay - DepDelay))
#' head(arrange(hflights, Dest, desc(ArrDelay)))
#'
#' # If you start with a tbl, you end up with a tbl
#' hflights2 <- as.tbl(hflights)
#' filter(hflights2, Month == 1, DayofMonth == 1, Dest == "DFW")
#' head(select(hflights2, Year:DayOfWeek))
#' summarise(hflights2, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#' head(mutate(hflights2, gained = ArrDelay - DepDelay))
#' head(arrange(hflights2, Dest, desc(ArrDelay)))
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
summarise.data.table <- function(.data, ...) {
  cols <- named_dots(...)
  list_call <- as.call(c(quote(list), named_dots(...)))
  call <- substitute(.data[, list_call])

  eval(call, parent.frame())
}

#' @export
summarise.tbl_dt <- function(.data, ...) {
  tbl_dt(
    summarise.data.table(.data, ...)
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
  for(col in names(cols)) {
    call <- substitute(data[, lhs := rhs],
      list(lhs = as.name(col), rhs = cols[[col]]))
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
select.data.table <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  input_vars <- vapply(input, as.character, character(1))

  .data[, input_vars, drop = FALSE, with = FALSE]
}

#' @export
select.tbl_dt <- function(.data, ...) {
  tbl_dt(
    select.data.table(.data, ...)
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
