#' Recode and replace values
#'
#' @description
#' TODO
#'
#' @seealso [case_when()]
#'
#' @name recode-and-replace-values
NULL

#' @export
#' @rdname recode-and-replace-values
recode_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  unmatched = "default",
  ptype = NULL
) {
  recode_values_with_envs(
    x = x,
    dots = list2(...),
    from = from,
    to = to,
    default = default,
    unmatched = unmatched,
    ptype = ptype,
    call = current_env(),
    default_env = caller_env(),
    dots_env = current_env()
  )
}

#' @export
#' @rdname recode-and-replace-values
replace_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL
) {
  obj_check_vector(x)

  default <- x
  ptype <- vec_ptype_finalise(vec_ptype(x))

  recode_values_with_envs(
    x = x,
    dots = list2(...),
    from = from,
    to = to,
    default = default,
    unmatched = "default",
    ptype = ptype,
    call = current_env(),
    default_env = caller_env(),
    dots_env = current_env()
  )
}

recode_values_with_envs <- function(
  x,
  dots,
  from,
  to,
  default,
  unmatched,
  ptype,
  call,
  default_env,
  dots_env
) {
  dots <- case_formula_evaluate(
    dots = dots,
    default_env = default_env,
    dots_env = dots_env,
    error_call = call,
    allow_named_dots = FALSE
  )

  implementation <- check_mutually_exclusive_arguments(
    dots = dots,
    from = from,
    to = to,
    from_arg = "from",
    to_arg = "to",
    call = call
  )

  switch(
    implementation,
    "vector" = {
      multiple_from <- obj_is_list(from)
      multiple_to <- obj_is_list(to)
    },
    "formula" = {
      multiple_from <- TRUE
      multiple_to <- TRUE
      from <- dots$lhs
      to <- dots$rhs
    }
  )

  vec_recode_values(
    x = x,
    from = from,
    to = to,
    default = default,
    unmatched = unmatched,
    multiple_from = multiple_from,
    multiple_to = multiple_to,
    x_arg = "x",
    from_arg = "from",
    to_arg = "to",
    default_arg = "default",
    ptype = ptype,
    call = call
  )
}

check_mutually_exclusive_arguments <- function(
  dots,
  from,
  to,
  from_arg,
  to_arg,
  call
) {
  has_dots <- length(dots$lhs) != 0L
  has_from <- !is.null(from)
  has_to <- !is.null(to)

  if (has_from && has_dots) {
    cli::cli_abort(
      "Can't supply both {.arg {from_arg}} and {.arg ...}.",
      call = call
    )
  }

  if (!has_from && !has_dots) {
    cli::cli_abort(
      "Must supply either {.arg ...} or both {.arg {from_arg}} and {.arg {to_arg}}.",
      call = call
    )
  }

  if (has_to && has_dots) {
    cli::cli_abort(
      "Can't supply both {.arg {to_arg}} and {.arg ...}.",
      call = call
    )
  }

  if ((has_from && !has_to) || (has_to && !has_from)) {
    cli::cli_abort(
      "Must supply both {.arg {from_arg}} and {.arg {to_arg}}.",
      call = call
    )
  }

  if (has_from) {
    "vector"
  } else {
    "formula"
  }
}
