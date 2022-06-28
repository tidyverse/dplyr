mutate_when <- function(data, when, caller_env, ..., error_call = caller_env()) {
  check_dots_empty0(...)

  when <- enquo(when)

  if (quo_is_null(when)) {
    # Not using `.when`
    return(NULL)
  }

  error_call <- dplyr_error_call(error_call)

  mutate_when_error_handler <- make_mutate_when_error_handler(when, error_call)

  withCallingHandlers(
    expr = mutate_when_eval(when, data),
    error = mutate_when_error_handler
  )
}

mutate_when_eval <- function(when, data) {
  # `when` is a quosure, and won't use `env`
  env <- empty_env()

  when <- eval_tidy(
    expr = when,
    data = data,
    env = env
  )

  if (!is.logical(when)) {
    stop_mutate_when_incompatible_type(when)
  }

  when_size <- vec_size(when)
  expected_size <- vec_size(data)

  if (when_size != expected_size) {
    stop_mutate_when_incompatible_size(when_size, expected_size)
  }

  if (anyNA(when)) {
    when[vec_equal_na(when)] <- FALSE
  }

  loc <- vec_as_location(when, n = when_size)

  list(
    flag = when,
    loc = loc
  )
}

make_mutate_when_error_handler <- function(when, error_call) {
  function(cnd) {
    when <- quo_as_label(when)
    header <- glue("Problem while computing `.when = {when}`.")

    bullets <- c(
      header,
      mutate_when_bullets(cnd)
    )

    abort(
      bullets,
      class = "dplyr:::mutate_when_error",
      parent = skip_internal_condition(cnd),
      bullets = bullets,
      call = error_call
    )
  }
}

mutate_when_bullets <- function(cnd) {
  UseMethod("mutate_when_bullets")
}

#' @export
mutate_when_bullets.default <- function(cnd) {
  character()
}

#' @export
`mutate_when_bullets.dplyr:::mutate_when_incompatible_size` <- function(cnd) {
  when_size <- cnd$when_size
  expected_size <- cnd$expected_size

  c(
    x = "`.when` must have the same size as `.data`.",
    i = glue("`.data` has size {expected_size}."),
    i = glue("`.when` has size {when_size}.")
  )
}

stop_mutate_when_incompatible_size <- function(when_size, expected_size) {
  abort(
    class = "dplyr:::mutate_when_incompatible_size",
    when_size = when_size,
    expected_size = expected_size
  )
}

#' @export
`mutate_when_bullets.dplyr:::mutate_when_incompatible_type` <- function(cnd) {
  when <- cnd$when

  c(
    x = glue("`.when` must evaluate to a logical vector, not {friendly_type_of(when)}.")
  )
}

stop_mutate_when_incompatible_type <- function(when) {
  abort(
    class = "dplyr:::mutate_when_incompatible_type",
    when = when
  )
}

# ------------------------------------------------------------------------------

group_data_when <- function(data, when) {
  UseMethod("group_data_when")
}

#' @export
group_data_when.data.frame <- function(data, when) {
  group_locs <- when$loc
  group_when_locs <- seq_along(group_locs)

  group_rows <- new_list_of(list(group_locs), ptype = integer())
  group_when_rows <- new_list_of(list(group_when_locs), ptype = integer())

  group_data <- new_data_frame(list(.rows = group_rows), n = 1L)

  list(
    data = group_data,
    rows = group_when_rows
  )
}

#' @export
group_data_when.tbl_df <- function(data, when) {
  out <- group_data_when.data.frame(data, when)
  out$data <- new_tibble(out$data, nrow = 1L)
  out
}

#' @export
group_data_when.grouped_df <- function(data, when) {
  group_data <- group_data(data)
  group_rows <- group_data[[".rows"]]

  n_groups <- vec_size(group_rows)
  group_locs <- vec_unchop(group_rows, ptype = integer(), name_spec = zap())

  group_when_locs <- vec_init(NA_integer_, n = vec_size(when$flag))
  group_when_locs[when$loc] <- seq_along(when$loc)

  # Reorder to line up with groups
  info <- vctrs::data_frame(
    group_when_locs = group_when_locs,
    when_flag = when$flag
  )
  info <- vec_slice(info, group_locs)
  group_when_locs <- info$group_when_locs
  when_flag <- info$when_flag

  group_sizes <- list_sizes(group_rows)
  group_ids <- vec_rep_each(seq_len(n_groups), group_sizes)

  # Filter by `when`
  info <- vctrs::data_frame(
    group_when_locs = group_when_locs,
    group_locs = group_locs,
    group_ids = group_ids
  )
  info <- vec_slice(info, when_flag)
  group_when_locs <- info$group_when_locs
  group_locs <- info$group_locs
  group_ids <- info$group_ids

  # Recreate chopped groups now that `when` has been applied.
  # `group_ids` is in sequential order so we can locate the endpoints
  # and chop with compact-seqs
  ends <- vec_locate_runs(group_ids, start = FALSE)

  info <- .Call(
    `dplyr_mutate_when_chop_runs`,
    group_locs,
    group_when_locs,
    ends
  )
  group_rows <- info[[1L]]
  group_when_rows <- info[[2L]]

  group_rows <- new_list_of(group_rows, ptype = integer())
  group_when_rows <- new_list_of(group_when_rows, ptype = integer())

  if (vec_size(ends) != n_groups) {
    # Resize `group_data` if we dropped groups when slicing with `when`
    group_data <- vec_slice(group_data, vec_slice(group_ids, ends))
  }

  group_data[[".rows"]] <- group_rows

  list(
    data = group_data,
    rows = group_when_rows
  )
}

#' @export
group_data_when.rowwise_df <- function(data, when) {
  group_data_when.grouped_df(data, when)
}
