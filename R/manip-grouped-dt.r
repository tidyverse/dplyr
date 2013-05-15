#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("data.table")) {
#' data("baseball", package = "plyr")
#' baseball_dt <- as.data.table(baseball)
#' players <- group_by(baseball_dt, id)
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' mutate(players, cyear = year - min(year) + 1)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # (for hopefully obvious reasons)
#' by_year <- mutate(players, cyear = year - min(year) + 1)
#' summarise(by_year, years = max(cyear))
#'
#' # You can also manually ungroup:
#' arrange(ungroup(by_year), id, year)
#' }
#' @name grouped_dt
NULL

#' @rdname grouped_dt
#' @export
#' @method filter grouped_dt
filter.grouped_dt <- function(.data, ...) {
  expr <- and_expr(dots(...))

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- deparse_all(.data$vars)

  call <- substitute(data[data[, .I[expr], by = vars]$V1])
  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = .data$vars
  )
}

#' @rdname grouped_dt
#' @export
#' @method summarise grouped_dt
summarise.grouped_dt <- function(.data, ...) {
  # Set keys, if needed
  keys <- deparse_all(.data$vars)
  if (!identical(keys, key(.data$obj))) {
    setkeyv(.data$obj, keys)
  }

  cols <- named_dots(...)
  list_call <- as.call(c(quote(list), named_dots(...)))
  call <- substitute(data[, list_call, by = vars])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- keys
  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = .data$vars
  )
}

#' @rdname grouped_dt
#' @export
#' @method mutate grouped_dt
mutate.grouped_dt <- function(.data, ..., inplace = FALSE) {
  data <- .data$obj
  # Set keys, if needed
  keys <- deparse_all(.data$vars)
  if (!identical(keys, key(.data$obj))) {
    setkeyv(.data$obj, keys)
  }
  if (!inplace) data <- copy(data)

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- data
  env$vars <- keys

  cols <- named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(cols)) {
    call <- substitute(data[, lhs := rhs, by = vars],
      list(lhs = as.name(col), rhs = cols[[col]]))
    eval(call, env)
  }

  grouped_dt(
    data = data,
    vars = .data$vars
  )
}

#' @rdname grouped_dt
#' @export
#' @method arrange grouped_dt
arrange.grouped_dt <- function(.data, ...) {
  keys <- deparse_all(.data$vars)

  order_call <- as.call(c(quote(order), .data$vars, dots(...)))
  call <- substitute(data[order_call])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj

  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = .data$vars
  )
}

#' @rdname grouped_dt
#' @export
#' @method select grouped_dt
select.grouped_dt <- function(.data, ...) {
  input <- var_eval(.data$obj, dots(...), parent.frame())
  out <- .data$obj[, input, drop = FALSE, with = FALSE]

  grouped_dt(
    data = out,
    vars = .data$vars
  )
}
