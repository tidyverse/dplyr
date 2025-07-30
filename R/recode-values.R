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
  dots <- case_formula_evaluate(
    dots = list2(...),
    default_env = caller_env(),
    dots_env = current_env(),
    error_call = current_env(),
    allow_named_dots = FALSE
  )

  implementation <- check_mutually_exclusive_arguments(
    dots = dots,
    from = from,
    to = to,
    from_arg = "from",
    to_arg = "to",
    call = current_env()
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
    call = current_env()
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
  dots <- case_formula_evaluate(
    dots = list2(...),
    default_env = caller_env(),
    dots_env = current_env(),
    error_call = current_env(),
    allow_named_dots = FALSE
  )

  implementation <- check_mutually_exclusive_arguments(
    dots = dots,
    from = from,
    to = to,
    from_arg = "from",
    to_arg = "to",
    call = current_env()
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

  vec_replace_values(
    x = x,
    from = from,
    to = to,
    multiple_from = multiple_from,
    multiple_to = multiple_to,
    x_arg = "x",
    from_arg = "from",
    to_arg = "to",
    call = current_env()
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
