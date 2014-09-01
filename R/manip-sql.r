#' @export
filter.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())

  update(.data, where = c(.data$where, input))
}

#' @export
arrange.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, order_by = c(input, .data$order_by))
}

#' @export
select_.tbl_sql <- function(.data, args) {
  args <- lazy::as.lazy_dots(args, parent.frame())
  vars <- select_vars_(tbl_vars(.data), args,
    include = as.character(groups(.data)))
  # Index into variables so that select can be applied multiple times
  # and after a mutate.
  idx <- match(vars, tbl_vars(.data))
  new_select <- .data$select[idx]
  names(new_select) <- names(vars)

  update(.data, select = new_select)
}

#' @export
rename.tbl_sql <- function(.data, ...) {
  vars <- rename_vars_(tbl_vars(.data), lazy::lazy_dots(...))
  # Index into variables so that select can be applied multiple times
  # and after a mutate.
  idx <- match(vars, tbl_vars(.data))
  new_select <- .data$select[idx]
  names(new_select) <- names(vars)

  update(.data, select = new_select)
}


#' @export
summarise.tbl_sql <- function(.data, ..., .collapse_result = TRUE) {
  input <- partial_eval(dots(...), .data, parent.frame())
  input <- auto_name(input)

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

  if (!.collapse_result) return(.data)
  # Technically, don't always need to collapse result because summarise + filter
  # could be expressed in SQL using HAVING, but that's the only dplyr operation
  # that can be, so would be a lot of extra work for minimal gain
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
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


#' @export
mutate.tbl_sql <- function(.data, ...) {
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
