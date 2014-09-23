#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ...,args variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("data.table") && require("nycflights13")) {
#' flights2 <- tbl_dt(flights)
#' by_dest <- group_by(flights2, dest)
#'
#' filter(by_dest, arr_delay == max(arr_delay, na.rm = TRUE))
#' summarise(by_dest, arr = mean(arr_delay, na.rm = TRUE))
#'
#' # Normalise arrival and departure delays by airport
#' scaled <- mutate(by_dest, arr_z = scale(arr_delay), dep_z = scale(dep_delay))
#' select(scaled, year:day, dest, arr_z:dep_z)
#'
#' arrange(by_dest, desc(arr_delay))
#' select(by_dest, -(day:tailnum))
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(flights, year, month, day)
#' by_month <- summarise(by_day, delayed = sum(arr_delay > 0, na.rm = TRUE))
#' by_month
#' summarise(by_month, delayed = sum(delayed))
#'
#' # You can also manually ungroup:
#' ungroup(by_day)
#' }
#' @name manip_grouped_dt
NULL

#' @rdname manip_grouped_dt
#' @export
filter.grouped_dt <- function(.data, ...) {
  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data
  env$vars <- deparse_all(groups(.data))

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  expr <- and_expr(dots(...))
  call <- substitute(data[, .I[expr], by = vars])
  indices <- eval(call, env)$V1
  out <- .data[indices[!is.na(indices)]]

  grouped_dt(
    data = out,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_dt
#' @export
summarise_.grouped_dt <- function(.data, dots) {
  dots <- lazyeval::as.lazy_dots(dots, parent.frame())

  # Replace n() with .N
  for (i in seq_along(dots)) {
    if (identical(dots[[i]]$expr, quote(n()))) {
      dots[[i]]$expr <- quote(.N)
    }
  }

  list_call <- lazyeval::make_call(quote(list), dots)
  call <- substitute(dt[, list_call, by = vars], list(list_call = list_call$expr))

  env <- dt_env(.data, parent.frame())
  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = drop_last(groups(.data))
  )
}

#' @rdname manip_grouped_dt
#' @export
mutate.grouped_dt <- function(.data, ..., inplace = FALSE) {
  data <- .data
  if (!inplace) data <- copy(data)

  env <- dt_env(data, parent.frame())
  cols <- named_dots(...)
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(cols)) {
    call <- substitute(dt[, lhs := rhs, by = vars],
      list(lhs = as.name(col), rhs = cols[[col]]))
    eval(call, env)
  }

  grouped_dt(
    data = data,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_dt
#' @export
arrange.grouped_dt <- function(.data, ...) {
  keys <- deparse_all(groups(.data))

  order_call <- as.call(c(quote(order), groups(.data), dots(...)))
  call <- substitute(data[order_call])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data

  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_dt
#' @export
select_.grouped_dt <- function(.data, args) {
  args <- lazyeval::as.lazy_dots(args, parent.frame())
  vars <- select_vars_(names(.data), args,
    include = as.character(groups(.data)))
  out <- .data[, vars, drop = FALSE, with = FALSE]
  setnames(out, names(vars))

  grouped_dt(
    data = out,
    vars = groups(.data)
  )
}

#' @rdname manip_grouped_dt
#' @export
rename.grouped_dt <- function(.data, ...) {
  vars <- rename_vars_(names(.data), lazyeval::lazy_dots(...))
  out <- .data[, vars, drop = FALSE, with = FALSE]
  setnames(out, names(vars))

  grouped_dt(
    data = out,
    vars = groups(.data)
  )
}
