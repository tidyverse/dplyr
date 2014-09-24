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
filter_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())

  expr <- lapply(dots, `[[`, "expr")
  call <- substitute(dt[expr, ], list(expr = and_expr(expr)))

  env <- dt_env(.data, lazyeval::common_env(dots))
  eval(call, env)
}

#' @export
filter_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
summarise_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)

  expr <- lazyeval::make_call(quote(list), dots)
  call <- substitute(dt[, expr], list(expr = expr$expr))

  env <- dt_env(.data, lazyeval::common_env(dots))
  eval(call, env)
}

#' @export
summarise_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
mutate_.data.table <- function(.data, ..., .dots, inplace = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
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
mutate.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
arrange_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())

  order <- lazyeval::make_call(quote(order), dots)
  call <- substitute(dt[order], list(order = order$expr))

  env <- dt_env(.data, order$env)
  eval(call, env)
}

#' @export
arrange_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
select_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- select_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
select_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
rename_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
rename_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}


#' @export
do.data.table <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @export
do.tbl_dt <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}
