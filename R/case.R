mutate_case <- function(.data, ...) {
  caller_env <- caller_env()
  error_call <- caller_env

  args <- list2(...)
  args <- Filter(function(x) !is.null(x), args)
  n_args <- length(args)

  good <- vapply(args, is_dplyr_when, logical(1))
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

    # Evaluate current element of `...` on the slice of `.data` we are updating
    updates <- dplyr_row_slice(.data, loc)
    updates <- mutate_cols(updates, dots = values, caller_env = caller_env)

    out <- df_update(out, loc, updates, error_call = error_call)
  }

  out
}

when <- function(.condition, ...) {
  condition <- enquo(.condition)

  # quos() not enquos(), for performance, since we only need to collect `...`
  values <- quos(..., .ignore_empty = "all", .named = NULL)

  structure(
    list(condition = condition, values = values),
    class = "dplyr_when"
  )
}

is_dplyr_when <- function(x) {
  inherits(x, "dplyr_when")
}
