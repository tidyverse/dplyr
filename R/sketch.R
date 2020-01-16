#' @export
sketch <- function(.data, ...) {
  UseMethod("sketch")
}

#' @export
sketch.data.frame <- function(.data, ...) {
  cols <- sketch_cols(.data, ...)

  res <- group_keys(.data)
  res[names(cols)] <- cols
  if (is_grouped_df(.data)) {
    res <- group_by(res, !!!head(groups(.data), -1))
  }
  res <- `class<-`(res, c("rowwise_df", class(res)))
  res
}

sketch_cols <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(..., .named = TRUE)
  dots_names <- names(dots)

  .size <- 1L
  chunks <- vector("list", length(dots))

  tryCatch({

    for (i in seq_along(dots)) {
      chunks[[i]] <- mask$eval_all(dots[[i]])
      mask$add(dots_names[i], chunks[[i]])
    }

  },
  simpleError = function(e) {
    stop_eval_tidy(e, index = i, dots = dots, fn = "sketch")
  })

  set_names(chunks, dots_names)
}
