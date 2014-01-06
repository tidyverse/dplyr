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
select.tbl_sql <- function(.data, ...) {
  input <- select_eval(dots(...), .data$select, parent.frame())
  update(.data, select = input)
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
  update(.data, select = c(.data$select, input))
}

#' @export
#' @rdname do
#' @param .chunk_size The size of each chunk to pull into R. If this number is
#'   too big, the process will be slow because R has to allocate and free a lot
#'   of memory. If it's too small, it will be slow, because of the overhead of
#'   talking to the database.
do.tbl_sql <- function(.data, .f, ..., .chunk_size = 1e4L) {
  group_by <- .data$group_by
  if (is.null(group_by)) stop("No grouping", call. = FALSE)

  gvars <- seq_along(group_by)
  # Create data frame of labels.
  labels_tbl <- update(.data,
    select = group_by,
    order_by = NULL)
  labels <- as.data.frame(labels_tbl)

  # Create ungrouped data frame suitable for chunked retrieval
  names(group_by) <- paste0("GRP_", seq_along(group_by))
  chunky <- update(.data,
    select = c(group_by, .data$select),
    order_by = c(unname(group_by), .data$order_by),
    group_by = NULL
  )

  # When retrieving in pages, there's no guarantee we'll get a complete group.
  # So we always assume the last group in the chunk is incomplete, and leave
  # it for the next. If the group size is large than chunk size, it may
  # take a couple of iterations to get the entire group, but that should
  # be an unusual situation.
  last_group <- NULL
  out <- vector("list", nrow(labels))
  i <- 0

  chunky$query$fetch_paged(.chunk_size, function(chunk) {
    if (!is.null(last_group)) chunk <- rbind(last_group, chunk)

    # Create an id for each group
    group_id <- id(chunk[gvars], drop = TRUE)
    n <- attr(group_id, "n")

    index <- split_indices(group_id, n)
    last_group <<- chunk[index[[length(index)]], , drop = FALSE]

    for (j in seq_len(n - 1)) {
      subs <- chunk[index[[j]], , drop = FALSE]
      out[[i + j]] <<- .f(subs, ...)
    }
    i <<- i + (n - 1)
  })

  # Process last group
  if (!is.null(last_group)) {
    out[[i + 1]] <- .f(last_group, ...)
  }

  labels$DO <- out
  grouped_df(labels, drop_last(groups(.data)))
}
