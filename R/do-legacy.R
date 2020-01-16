old_do <- function(.data, ...) {
  UseMethod("old_do")
}

#' @export
old_do.NULL <- function(.data, ...) {
  NULL
}


#' @export
old_do.grouped_df <- function(.data, ...) {
  index <- group_rows(.data)
  labels <- select(group_data(.data), -last_col())
  attr(labels, ".drop") <- NULL

  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)

  args <- enquos(...)
  named <- named_args(args)
  mask <- new_data_mask(new_environment())

  n <- length(index)
  m <- length(args)

  # Special case for zero-group/zero-row input
  if (n == 0) {
    if (named) {
      out <- rep_len(list(list()), length(args))
      out <- set_names(out, names(args))
      out <- label_output_list(labels, out, groups(.data))
    } else {
      env_bind_do_pronouns(mask, group_data)
      out <- eval_tidy(args[[1]], mask)
      out <- out[0, , drop = FALSE]
      out <- label_output_dataframe(labels, list(list(out)), group_vars(.data), group_by_drop_default(.data))
    }
    return(out)
  }

  # Add pronouns with active bindings that resolve to the current
  # subset. `_i` is found in environment of this function because of
  # usual scoping rules.
  group_slice <- function(value) {
    if (missing(value)) {
      group_data[index[[`_i`]], , drop = FALSE]
    } else {
      group_data[index[[`_i`]], ] <<- value
    }
  }
  env_bind_do_pronouns(mask, group_slice)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(eval_tidy(args[[j]], mask))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(labels, out, group_vars(.data), group_by_drop_default(.data))
  } else {
    label_output_list(labels, out, group_vars(.data))
  }
}

#' @export
old_do.data.frame <- function(.data, ...) {
  args <- enquos(...)
  named <- named_args(args)

  # Create custom data mask with `.` pronoun
  mask <- new_data_mask(new_environment())
  env_bind_do_pronouns(mask, .data)

  if (!named) {
    out <- eval_tidy(args[[1]], mask)
    if (!inherits(out, "data.frame")) {
      bad("Result must be a data frame, not {fmt_classes(out)}")
    }
  } else {
    out <- map(args, function(arg) list(eval_tidy(arg, mask)))
    names(out) <- names(args)
    out <- tibble::as_tibble(out, validate = FALSE)
  }

  out
}



#' @export
old_do.rowwise_df <- function(.data, ...) {
  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)

  args <- enquos(...)
  named <- named_args(args)

  # Create new environment, inheriting from parent, with an active binding
  # for . that resolves to the current subset. `_i` is found in environment
  # of this function because of usual scoping rules.
  mask <- new_data_mask(new_environment())
  current_row <- function() lapply(group_data[`_i`, , drop = FALSE], "[[", 1)
  env_bind_do_pronouns(mask, current_row)

  n <- nrow(.data)
  m <- length(args)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(eval_tidy(args[[j]], mask))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(NULL, out, groups(.data), group_by_drop_default(.data))
  } else {
    label_output_list(NULL, out, groups(.data))
  }
}

# Helper functions -------------------------------------------------------------

env_bind_do_pronouns <- function(env, data) {
  if (is_function(data)) {
    bind <- env_bind_active
  } else {
    bind <- env_bind
  }

  # Use `:=` for `.` to avoid partial matching with `.env`
  bind(env, "." := data, .data = data)
}

label_output_dataframe <- function(labels, out, groups, .drop) {
  data_frame <- vapply(out[[1]], is.data.frame, logical(1))
  if (any(!data_frame)) {
    bad("Results {bad} must be data frames, not {first_bad_class}",
      bad = fmt_comma(which(!data_frame)),
      first_bad_class = fmt_classes(out[[1]][[which.min(data_frame)]])
    )
  }

  rows <- vapply(out[[1]], nrow, numeric(1))
  out <- bind_rows(out[[1]])

  if (!is.null(labels)) {
    # Remove any common columns from labels
    labels <- labels[setdiff(names(labels), names(out))]

    # Repeat each row to match data
    labels <- labels[rep(seq_len(nrow(labels)), rows), , drop = FALSE]
    rownames(labels) <- NULL

    grouped_df(bind_cols(labels, out), groups, .drop)
  } else {
    rowwise(out)
  }
}

label_output_list <- function(labels, out, groups) {
  if (!is.null(labels)) {
    labels[names(out)] <- out
    rowwise(labels)
  } else {
    class(out) <- "data.frame"
    attr(out, "row.names") <- .set_row_names(length(out[[1]]))
    rowwise(out)
  }
}

named_args <- function(args) {
  # Arguments must either be all named or all unnamed.
  named <- sum(names2(args) != "")
  if (!(named == 0 || named == length(args))) {
    abort("Arguments must either be all named or all unnamed")
  }
  if (named == 0 && length(args) > 1) {
    bad("Can only supply one unnamed argument, not {length(args)}")
  }

  # Check for old syntax
  if (named == 1 && names(args) == ".f") {
    abort("do syntax changed in dplyr 0.2. Please see documentation for details")
  }

  named != 0
}
