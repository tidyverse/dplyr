when <- function(.condition, ...) {
  .condition <- enquo(.condition)

  # quos() not enquos(), for performance, since we only need to collect `...`
  values <- quos(..., .ignore_empty = "all", .named = NULL)
  names <- names2(values)
  named <- names != ""

  n_values <- length(values)
  n_named <- sum(named)

  if (n_values > 1L && n_values != n_named) {
    unnamed <- !named
    unnamed <- which(unnamed)
    unnamed <- unnamed[[1L]]

    message <- c(
      "All `...` must be named when >1 inputs are supplied.",
      i = glue("{n_values} inputs were supplied."),
      i = glue("{n_named} inputs were named."),
      i = glue("Input {unnamed} of `...` is not named.")
    )

    abort(message)
  }

  structure(
    list(condition = .condition, values = values),
    class = "funs_when"
  )
}

is_funs_when <- function(x) {
  inherits(x, "funs_when")
}

case <- function(...,
                 .default = NULL,
                 .ptype = NULL,
                 .size = NULL) {
  args <- list2(...)
  args <- Filter(function(x) !is.null(x), args)
  n_args <- length(args)

  good <- vapply(args, is_funs_when, logical(1))
  if (!all(good)) {
    bad <- !good
    bad <- which(bad)
    bad <- bad[[1L]]

    message <- c(
      "All `...` inputs must be `when()` results.",
      i = glue("Input {bad} is not a `when()` result.")
    )

    abort(message)
  }

  args_conditions <- vector("list", length = n_args)
  args_values <- vector("list", length = n_args)

  for (i in seq_len(n_args)) {
    arg <- args[[i]]

    condition <- arg$condition
    values <- arg$values

    condition <- eval_tidy(condition)
    values <- lapply(values, eval_tidy)

    if (length(values) == 1L && is.null(names(values))) {
      # `when()` guaranteed that there is only 1 input in this case
      values <- values[[1L]]
      vec_assert(values)
    } else {
      size <- vec_size(condition)
      list_check_all_vectors(values)
      values <- vec_recycle_common(!!!values, .size = size)
      values <- new_data_frame(values, n = size)
    }

    args_conditions[[i]] <- condition
    args_values[[i]] <- values
  }

  args <- vec_interleave(args_conditions, args_values)

  vec_case_when(!!!args, .default = .default, .ptype = .ptype, .size = .size)
}

revise2 <- function(.data, ...) {
  caller_env <- caller_env()

  args <- list2(...)
  args <- Filter(function(x) !is.null(x), args)
  n_args <- length(args)

  good <- vapply(args, is_funs_when, logical(1))
  if (!all(good)) {
    bad <- !good
    bad <- which(bad)
    bad <- bad[[1L]]

    message <- c(
      "All `...` inputs must be `when()` results.",
      i = glue("Input {bad} is not a `when()` result.")
    )

    abort(message)
  }

  size <- vec_size(.data)
  unused <- vec_rep(TRUE, size)

  out <- .data

  for (i in seq_len(n_args)) {
    arg <- args[[i]]

    condition <- arg$condition
    values <- arg$values
    values <- dplyr_quosures(!!!values)

    # Evaluate `condition` on all of `.data`.
    # This handles `NA`s for us, and converts them to `FALSE`.
    condition <- filter_rows(.data, !!condition, caller_env = caller_env)

    # Only update in locations we haven't updated before
    loc <- unused & condition
    loc <- vec_as_location(loc, n = size)

    # Mark locations as used
    unused[loc] <- FALSE

    # Evaluate current subset of `...` on the slice of `.data` we are updating
    updates <- dplyr_row_slice(.data, loc)
    updates <- mutate_cols(updates, dots = values, caller_env = caller_env)

    out <- df_update(out, loc, updates)
  }

  out
}
