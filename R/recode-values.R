#' Recode and replace values
#'
#' @description
#' This function allows you to vectorise multiple [switch()] statements. Each
#' case is evaluated sequentially and the first match for each element
#' determines the corresponding value in the output vector. If no cases match,
#' the `.default` is used.
#'
#' `case_match()` is an R equivalent of the SQL "simple" `CASE WHEN` statement.
#'
#' ## Connection to `case_when()`
#'
#' While [case_when()] uses logical expressions on the left-hand side of the
#' formula, `recode_values()` uses values to match against `x` with. The
#' following two statements are roughly equivalent:
#'
#' ```
#' case_when(
#'   x %in% c("a", "b") ~ 1,
#'   x %in% "c" ~ 2,
#'   x %in% c("d", "e") ~ 3
#' )
#'
#' recode_values(
#'   x,
#'   c("a", "b") ~ 1,
#'   "c" ~ 2,
#'   c("d", "e") ~ 3
#' )
#' ```
#'
#' @param x A vector to match against.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas: `old_values ~ new_value`. The right hand side (RHS) determines
#'   the output value for all values of `.x` that match the left hand side
#'   (LHS).
#'
#'   The LHS must evaluate to the same type of vector as `.x`. It can be any
#'   length, allowing you to map multiple `.x` values to the same RHS value.
#'   If a value is repeated in the LHS, i.e. a value in `.x` matches to
#'   multiple cases, the first match is used.
#'
#'   The RHS inputs will be coerced to their common type. Each RHS input will be
#'   [recycled][vctrs::theory-faq-recycling] to the size of `.x`.
#'
#' @param .default The value used when values in `.x` aren't matched by any of
#'   the LHS inputs. If `NULL`, the default, a missing value will be used.
#'
#'   `.default` is [recycled][vctrs::theory-faq-recycling] to the size of
#'   `.x`.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   not supplied, the output type will be taken from the common type of
#'   all RHS inputs and `.default`.
#'
#' @return
#' A vector with the same size as `.x` and the same type as the common type of
#' the RHS inputs and `.default` (if not overridden by `.ptype`).
#'
#' @seealso [case_when()]
#'
#' @name recode-and-replace-values
#' @examples
#' x <- c("a", "b", "a", "d", "b", NA, "c", "e")
#'
#' # `recode_values()` acts like a vectorized `switch()`.
#' # Unmatched values "fall through" as a missing value.
#' recode_values(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4
#' )
#'
#' # Missing values can be matched exactly, and `default` can be used to
#' # control the value used for unmatched values of `x`
#' recode_values(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4,
#'   NA ~ 0,
#'   default = 100
#' )
#'
#' # Input values can be grouped into the same expression to map them to the
#' # same output value
#' recode_values(
#'   x,
#'   c("a", "b") ~ "low",
#'   c("c", "d", "e") ~ "high"
#' )
#'
#' # `recode_values()` isn't limited to character input:
#' y <- c(1, 2, 1, 3, 1, NA, 2, 4)
#'
#' recode_values(
#'   y,
#'   c(1, 3) ~ "odd",
#'   c(2, 4) ~ "even",
#'   .default = "missing"
#' )
#'
#' # `replace_values()` replaces selected values, leaving everything
#' # else as is
#' replace_values(y, NA ~ 0)
#'
#' starwars %>%
#'   mutate(
#'     # Replace missings, but leave everything else alone
#'     hair_color = replace_values(hair_color, NA ~ "unknown"),
#'     # Replace some, but not all, of the species
#'     species = replace_values(
#'       species,
#'       "Human" ~ "Humanoid",
#'       "Droid" ~ "Robot",
#'       c("Wookiee", "Ewok") ~ "Hairy"
#'     ),
#'     .keep = "used"
#'   )
NULL

#' @export
#' @rdname recode-and-replace-values
recode_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  ptype = NULL
) {
  recode_values_with_envs(
    x = x,
    dots = list2(...),
    from = from,
    to = to,
    default = default,
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
  ptype,
  call,
  default_env,
  dots_env
) {
  if (!is_null(names(dots))) {
    abort("`...` can't be named.", call = call)
  }

  dots <- case_formula_evaluate(
    dots = dots,
    default_env = default_env,
    dots_env = dots_env,
    error_call = call
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
