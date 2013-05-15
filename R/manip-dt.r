#' Data manipulation for data tables.
#'
#' @examples
#' if (require("data.table")) {
#' data("baseball", package = "plyr")
#'
#' # If you start with a data table, you end up with a data table
#' baseball <- as.data.table(baseball)
#' filter(baseball, year > 2005, g > 130)
#' head(select(baseball, id:team))
#' summarise(baseball, g = mean(g), n = count())
#' head(mutate(baseball, rbi = r / ab, rbi2 = rbi ^ 2))
#' head(arrange(baseball, id, desc(year)))
#'
#' # If you start with a source, you end up with a source
#' baseball_s <- as.source(baseball)
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
#' @method filter data.table
filter.data.table <- function(.data, ...) {
  expr <- and_expr(dots(...))
  call <- substitute(.data[expr, ])

  eval(call, parent.frame())
}

#' @S3method filter source_dt
filter.source_dt <- function(.data, ...) {
  source_dt(
    filter.data.table(.data$obj, ...),
    .data$name
  )
}

#' @rdname manip_dt
#' @export
#' @method summarise data.table
summarise.data.table <- function(.data, ...) {
  cols <- named_dots(...)
  list_call <- as.call(c(quote(list), named_dots(...)))
  call <- substitute(.data[, list_call])

  eval(call, parent.frame())
}

#' @S3method summarise source_dt
summarise.source_dt <- function(.data, ...) {
  source_dt(
    summarise.data.table(.data$obj, ...),
    .data$name
  )
}

#' @rdname manip_dt
#' @export
#' @method mutate data.table
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

#' @S3method mutate source_dt
mutate.source_dt <- function(.data, ...) {
  source_dt(
    mutate.data.table(.data$obj, ...),
    .data$name
  )
}

#' @rdname manip_dt
#' @export
#' @method arrange data.table
arrange.data.table <- function(.data, ...) {
  call <- substitute(data[order(...)])
  env <- new.env(parent = parent.frame(), size = 1L)
  env$data <- .data
  out <- eval(call, env)

  eval(call, env)
}

#' @S3method arrange source_dt
arrange.source_dt <- function(.data, ...) {
  source_dt(
    arrange.data.table(.data$obj, ...),
    .data$name
  )
}

#' @rdname manip_dt
#' @export
#' @method select data.table
select.data.table <- function(.data, ...) {
  input <- var_eval(.data, dots(...), parent.frame())
  .data[, input, drop = FALSE, with = FALSE]
}

#' @S3method select source_dt
select.source_dt <- function(.data, ...) {
  source_dt(
    select.data.table(.data$obj, ...),
    .data$name
  )
}
