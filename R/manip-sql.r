#' @S3method filter tbl_sql
filter.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, where = c(.data$where, input))
}

#' @S3method arrange tbl_sql
arrange.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  update(.data, order_by = c(input, .data$order_by))
}

#' @S3method select tbl_sql
select.tbl_sql <- function(.data, ...) {
  input <- select_eval(dots(...), .data$select, parent.frame())
  update(.data, select = input)
}

#' @S3method summarise tbl_sql
summarise.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  input <- auto_name(input)
  
  .data <- collapse_if_needed(.data)
  .data <- update(.data, select = input, summarise = TRUE)
  new_vars <- lapply(names(input), as.name)
  
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}

#' @S3method mutate tbl_sql
mutate.tbl_sql <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$new_vars <- TRUE
  update(.data, select = c(.data$select, input))
}

#' @export
#' @method do tbl_sql
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
    last_group <<- chunk[last(index), , drop = FALSE]

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
