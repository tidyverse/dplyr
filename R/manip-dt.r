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
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
filter_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
filter_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  env <- lazyeval::common_env(.dots)

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  expr <- lapply(dots, `[[`, "expr")
  j <- substitute(list(`_row` = .I[expr]), list(expr = and_expr(expr)))
  indices <- dt_subset(.data, , j, env)$`_row`

  .data[indices[!is.na(indices)]]
}

# Summarise --------------------------------------------------------------------

#' @export
summarise_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
summarise_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
summarise_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  j <- lazyeval::make_call(quote(list), dots)
  dt_subset(.data, , j$expr, env = j$env)
}

# Mutate -----------------------------------------------------------------------

#' @export
mutate_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
mutate_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
mutate_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  names <- lapply(names(dots), as.name)

  # Never want to modify in place
  .data <- data.table::copy(.data)

  for(i in seq_along(dots)) {
    # For each new variable, generate a call of the form df[, new := expr]
    j <- substitute(lhs := rhs, list(lhs = names[[i]], rhs = dots[[i]]$expr))
    .data <- dt_subset(.data, , j, dots[[i]]$env)
  }

  .data
}

# Arrange ----------------------------------------------------------------------

#' @export
arrange_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
arrange_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
arrange_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  groups <- lazyeval::as.lazy_dots(groups(.data),
    env = lazyeval::common_env(dots))
  i <- lazyeval::make_call(quote(order), c(groups, dots))

  dt_subset(.data, i$expr, , env = i$env)
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
