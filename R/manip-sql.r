#' @export
filter_.tbl_sql <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  input <- partial_eval(dots, .data)

  update(.data, where = c(.data$where, input))
}

#' @export
arrange_.tbl_sql <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  input <- partial_eval(dots, .data)

  update(.data, order_by = c(input, .data$order_by))
}

#' @export
select_.tbl_sql <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- select_vars_(tbl_vars(.data), dots,
    include = as.character(groups(.data)))

  # Index into variables so that select can be applied multiple times
  # and after a mutate.
  idx <- match(vars, tbl_vars(.data))
  new_select <- .data$select[idx]
  names(new_select) <- names(vars)

  update(.data, select = new_select)
}

#' @export
rename_.tbl_sql <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame())
  vars <- rename_vars_(tbl_vars(.data), dots)

  # Index into variables so that select can be applied multiple times
  # and after a mutate.
  idx <- match(vars, tbl_vars(.data))
  new_select <- .data$select[idx]
  names(new_select) <- names(vars)

  update(.data, select = new_select)
}

#' @export
summarise_.tbl_sql <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., env = parent.frame(), all_named = TRUE)
  input <- partial_eval(dots, .data)

  # Effect of previous operations on summarise:
  # * select: none
  # * filter: none, just modifies WHERE (which is applied before)
  # * mutate: need to be precomputed so new select can use
  # * arrange: intersection with new variables preserved
  if (.data$mutate) {
    .data <- collapse(.data)
  }

  .data$summarise <- TRUE
  .data <- update(.data, select = c(.data$group_by, input))

  # Technically, don't always need to collapse result because summarise + filter
  # could be expressed in SQL using HAVING, but that's the only dplyr operation
  # that can be, so would be a lot of extra work for minimal gain
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}

#' @export
mutate_.tbl_sql <- function(.data, ..., .dots) {
  input <- partial_eval(dots(...), .data, parent.frame())
  input <- auto_name(input)

  .data$mutate <- TRUE
  new <- update(.data, select = c(.data$select, input))
  # If we're creating a variable that uses a window function, it's
  # safest to turn that into a subquery so that filter etc can use
  # the new variable name
  if (uses_window_fun(input, .data)) {
    collapse(new)
  } else {
    new
  }
}

#' @export
regroup.tbl_sql <- function(x, value) {
  if (!all_apply(value, is.name)) {
    stop("May only group by variable names, not expressions", call. = FALSE)
  }

  # Effect of group_by on previous operations:
  # * select: none
  # * filter: changes frame of window functions
  # * mutate: changes frame of window functions
  # * arrange: if present, groups inserted as first ordering
  needed <- (x$mutate && uses_window_fun(x$select, x)) ||
    uses_window_fun(x$filter, x)
  if (!is.null(x$order_by)) {
    arrange <- c(x$group_by, x$order_by)
  } else {
    arrange <- NULL
  }

  if (needed) {
    x <- collapse(update(x, order_by = NULL))
  }
  update(x, group_by = unname(value), order_by = arrange)
}
