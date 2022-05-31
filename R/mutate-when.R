mutate_when <- function(.data, ...) {
  caller_env <- caller_env()

  dots <- enquos(..., .ignore_empty = "trailing")

  names <- names2(dots)
  loc_conditions <- names == ""

  if (length(loc_conditions) >= 1L && is_false(loc_conditions[[1L]])) {
    # Required for the `cumsum()` below to work correctly
    abort("The first `...` value must be an unnamed logical condition.")
  }

  conditions <- dots[loc_conditions]
  n_conditions <- length(conditions)

  # Isolate the values that go with each `condition`
  loc <- seq_along(dots)
  id <- cumsum(loc_conditions)

  # First location of each group points to the condition itself,
  # the rest are the "updates" that go with it
  groups <- vec_split(loc, id)$val
  values <- map(groups, function(group) dots[group[-1L]])

  size <- vec_size(.data)
  unused <- vec_rep(TRUE, size)

  for (i in seq_len(n_conditions)) {
    condition <- conditions[[i]]
    value <- values[[i]]
    value <- dplyr_quosures(!!!value)

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
    updates <- mutate_cols(updates, dots = value, caller_env = caller_env)

    .data <- df_update(.data, loc, updates)
  }

  .data
}

# Take a data frame and update it at `loc` with `revisions`.
# Similar in spirit to:
# x[loc, names(revisions)] <- revisions
# but built from first principles to only use `dplyr_col_modify()`.
df_update <- function(x, loc, updates, error_call = caller_env()) {
  if (any(map_lgl(updates, is.null))) {
    abort("Can't delete columns when using `mutate_when()`.", call = error_call)
  }

  names <- names(updates)
  exists <- names %in% names(x)

  cols <- vector("list", length = length(updates))
  cols <- set_names(cols, names)

  # Avoid any subclass `[[` methods
  old <- unclass(x)

  for (i in seq_along(cols)) {
    name <- names[[i]]
    update <- updates[[i]]

    if (exists[[i]]) {
      col <- old[[name]]
    } else {
      col <- vec_init(update, n = vec_size(x))
    }

    cols[[i]] <- vec_assign(col, loc, update, x_arg = name)
  }

  dplyr_col_modify(x, cols)
}
