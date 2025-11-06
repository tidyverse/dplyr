#' @export
filter_out <- function(.data, ..., .by = NULL) {
  check_by_typo(...)
  UseMethod("filter_out")
}

#' @export
filter_out.data.frame <- function(.data, ..., .by = NULL) {
  retain_impl(
    data = .data,
    ...,
    by = {{ .by }},
    invert = TRUE,
    verb = "filter_out",
    error_call = current_env()
  )
}

retain_impl <- function(
  data,
  ...,
  by,
  invert,
  verb,
  error_call
) {
  check_dots_unnamed(call = error_call)

  by <- compute_by(
    by = {{ by }},
    data = data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = error_call
  )

  dots <- dplyr_quosures(...)

  condition <- retain_rows(
    data = data,
    dots = dots,
    by = by,
    invert = invert,
    verb = verb,
    error_call = error_call
  )

  # TODO:
  # Not entirely sure about `preserve`. It only affects existing
  # `<grouped_df>`s that are passed in, so has no affect with the
  # typical `.by` case. Is it possible we could just not support
  # grouped data frames and not have this argument?
  dplyr_row_slice(data, condition, preserve = TRUE)
}

retain_rows <- function(
  data,
  dots,
  by,
  invert,
  verb,
  error_call
) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, verb, error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  # Expand `pick()`, `if_any()`, and `if_all()` where possible
  # for efficiency
  dots <- retain_expand(
    dots = dots,
    mask = mask,
    error_call = error_call
  )

  # TODO: Needs lots of error management

  # Updated in place on the C side, used in error messaging
  dot_index <- 0L

  # Evaluate all `dots` across all groups
  dots_results <- mask$eval_quos(dots, dot_index)

  indices <- mask$get_rows()
  size <- mask$get_size()

  conditions <- map(dots_results, function(dot_results) {
    list_combine(dot_results, indices = indices, size = size, ptype = logical())
  })

  condition <- list_pall(
    conditions,
    missing = FALSE,
    size = size
  )

  if (invert) {
    # TODO: Faster `lgl_invert(condition)`
    condition <- !condition
  }

  condition
}

# TODO: Use simpler version after if_any/all updates

# - Expands `pick()` calls where possible
# - Expands `if_any()` and `if_all()` calls where possible
retain_expand <- function(dots, mask, error_call) {
  # Local environment for tracking the `index` of the currently
  # evaluating expression for error messaging purposes
  env_context <- env()

  retain_expand_one <- function(dot, index) {
    env_context$index <- index
    dot <- expand_pick(dot, mask)
    dot <- expand_if_across(dot)
    dot
  }

  local_error_context(dots, i = 0L, mask = mask)

  dots <- withCallingHandlers(
    expr = {
      map2(dots, seq_along(dots), retain_expand_one)
    },
    error = function(cnd) {
      index <- env_context$index
      poke_error_context(dots, index, mask = mask)
      abort(cnd_bullet_header("expand"), call = error_call, parent = cnd)
    }
  )

  dots <- list_flatten(dots)

  new_quosures(dots)
}
