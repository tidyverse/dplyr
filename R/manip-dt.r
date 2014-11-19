# Filter -----------------------------------------------------------------------

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

#' @export
filter_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  expr <- lapply(dots, `[[`, "expr")
  call <- substitute(dt[, .I[expr], by = vars], list(expr = and_expr(expr)))

  env <- dt_env(.data, lazyeval::common_env(dots))
  indices <- eval(call, env)$V1
  out <- .data[indices[!is.na(indices)]]

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
filter_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  expr <- lapply(dots, `[[`, "expr")
  call <- substitute(dt[expr, ], list(expr = and_expr(expr)))

  env <- dt_env(.data, lazyeval::common_env(dots))
  eval(call, env)
}

#' @export
filter_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Summarise --------------------------------------------------------------------

#' @export
summarise_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

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

  grouped_dt(out, drop_last(groups(.data)), copy = FALSE)
}

#' @export
summarise_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  expr <- lazyeval::make_call(quote(list), dots)
  call <- substitute(dt[, expr], list(expr = expr$expr))

  env <- dt_env(.data, lazyeval::common_env(dots))
  eval(call, env)
}

#' @export
summarise_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Mutate -----------------------------------------------------------------------

#' @export
mutate_.grouped_dt <- function(.data, ..., .dots, inplace = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (!inplace) .data <- data.table::copy(.data)

  env <- dt_env(.data, lazyeval::common_env(dots))
  # For each new variable, generate a call of the form df[, new := expr]
  for(col in names(dots)) {
    call <- substitute(dt[, lhs := rhs, by = vars],
      list(lhs = as.name(col), rhs = dots[[col]]$expr))
    eval(call, env)
  }

  grouped_dt(.data, groups(.data), copy = FALSE)
}


#' @export
mutate_.data.table <- function(.data, ..., .dots, inplace = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (!inplace) .data <- data.table::copy(.data)

  env <- dt_env(.data, lazyeval::common_env(dots))

  names <- lapply(names(dots), as.name)
  # For each new variable, generate a call of the form df[, new := expr]
  for(i in seq_along(dots)) {
    call <- substitute(dt[, lhs := rhs],
      list(lhs = names[[i]], rhs = dots[[i]]$expr))
    eval(call, env)
  }

  .data
}

#' @export
mutate_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Arrange ----------------------------------------------------------------------

#' @export
arrange_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  groups <- lazyeval::as.lazy_dots(groups(.data),
    env = lazyeval::common_env(dots))

  order <- lazyeval::make_call(quote(order), c(groups, dots))
  call <- substitute(dt[order, , ], list(order = order$expr))

  env <- dt_env(.data, order$env)
  out <- eval(call, env)

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
arrange_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  order <- lazyeval::make_call(quote(order), dots)
  call <- substitute(dt[order], list(order = order$expr))

  env <- dt_env(.data, order$env)
  eval(call, env)
}

#' @export
arrange_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Select -----------------------------------------------------------------------

#' @export
select_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots,
    include = as.character(groups(.data)))
  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
select_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
select_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Rename -----------------------------------------------------------------------

#' @export
rename_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
rename_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
rename_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}


# Slice -------------------------------------------------------------------

#' @export
slice_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
slice_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
slice_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  env <- lazyeval::common_env(dots)

  j <- substitute(.SD[rows], list(rows = dots[[1]]$expr))
  dt_subset(.data, , j, env)
}

# Do ---------------------------------------------------------------------------

#' @export
do_.data.table <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @export
do_.tbl_dt <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}
