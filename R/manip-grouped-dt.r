#' Data manipulation for grouped data tables.
#'
#' @param .data a data table
#' @param ... variables interpreted in the context of \code{.data}
#' @param inplace if \code{FALSE} (the default) the data frame will be copied
#'   prior to modification to avoid changes propagating via reference.
#' @examples
#' if (require("data.table")) {
#' hlights2 <- tbl_dt(hlights)
#' by_dest <- group_by(hflights2, Dest)
#
#' filter(by_dest, ArrDelay == max(ArrDelay))
#' summarise(by_dest, arr = mean(ArrDelay, na.rm = TRUE))
#' 
#' # Normalise arrival and departure delays by airport
#' scaled <- mutate(by_dest, arr_z = scale(ArrDelay), dep_z = scale(DepDelay))
#' select(scaled, Year:DayOfWeek, Dest, arr_z:dep_z)
#' 
#' arrange(by_dest, desc(ArrDelay))
#' select(by_dest, -(DayOfWeek:TailNum))
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(hflights, Year, Month, DayofMonth)
#' by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
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

#' @rdname manip_grouped_dt
#' @export
#' @method summarise grouped_dt
summarise.grouped_dt <- function(.data, ...) {
  # Set keys, if needed
  keys <- deparse_all(.data$vars)
  if (!identical(keys, key(.data$obj))) {
    setkeyv(.data$obj, keys)
  }

  cols <- named_dots(...)
  # Replace n() with .N
  for (i in seq_along(cols)) {
    if (identical(cols[[i]], quote(n()))) {
      cols[[i]] <- quote(.N)
    }
  }
  
  list_call <- as.call(c(quote(list), cols))
  call <- substitute(data[, list_call, by = vars])

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- keys
  out <- eval(call, env)

  grouped_dt(
    data = out,
    vars = .data$vars[-length(.data$vars)]
  )
}

#' @rdname manip_grouped_dt
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

#' @rdname manip_grouped_dt
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

#' @rdname manip_grouped_dt
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


#' @S3method do grouped_dt
do.grouped_dt <- function(.data, .f, ...) {
  keys <- deparse_all(.data$vars)
  setkeyv(.data$obj, keys)

  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data$obj
  env$vars <- keys
  env$f <- .f

  call <- substitute(data[, list(out = list(f(.SD, ...))), by = vars])
  eval(call, env)$out
}
