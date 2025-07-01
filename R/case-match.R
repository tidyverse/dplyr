#' A general vectorised `switch()`
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
#' formula, `case_match()` uses values to match against `.x` with. The following
#' two statements are roughly equivalent:
#'
#' ```
#' case_when(
#'   x %in% c("a", "b") ~ 1,
#'   x %in% "c" ~ 2,
#'   x %in% c("d", "e") ~ 3
#' )
#'
#' case_match(
#'   x,
#'   c("a", "b") ~ 1,
#'   "c" ~ 2,
#'   c("d", "e") ~ 3
#' )
#' ```
#'
#' @param .x A vector to match against.
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
#' @export
#' @examples
#' x <- c("a", "b", "a", "d", "b", NA, "c", "e")
#'
#' # `case_match()` acts like a vectorized `switch()`.
#' # Unmatched values "fall through" as a missing value.
#' case_match(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4
#' )
#'
#' # Missing values can be matched exactly, and `.default` can be used to
#' # control the value used for unmatched values of `.x`
#' case_match(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4,
#'   NA ~ 0,
#'   .default = 100
#' )
#'
#' # Input values can be grouped into the same expression to map them to the
#' # same output value
#' case_match(
#'   x,
#'   c("a", "b") ~ "low",
#'   c("c", "d", "e") ~ "high"
#' )
#'
#' # `case_match()` isn't limited to character input:
#' y <- c(1, 2, 1, 3, 1, NA, 2, 4)
#'
#' case_match(
#'   y,
#'   c(1, 3) ~ "odd",
#'   c(2, 4) ~ "even",
#'   .default = "missing"
#' )
#'
#' # Setting `.default` to the original vector is a useful way to replace
#' # selected values, leaving everything else as is
#' case_match(y, NA ~ 0, .default = y)
#'
#' starwars %>%
#'   mutate(
#'     # Replace missings, but leave everything else alone
#'     hair_color = case_match(hair_color, NA ~ "unknown", .default = hair_color),
#'     # Replace some, but not all, of the species
#'     species = case_match(
#'       species,
#'       "Human" ~ "Humanoid",
#'       "Droid" ~ "Robot",
#'       c("Wookiee", "Ewok") ~ "Hairy",
#'       .default = species
#'     ),
#'     .keep = "used"
#'   )
case_match <- function(
  .x,
  ...,
  .from = NULL,
  .to = NULL,
  .default = NULL,
  .ptype = NULL
) {
  case_match_with_envs(
    x = .x,
    dots = list2(...),
    from = .from,
    to = .to,
    default = .default,
    ptype = .ptype,
    call = current_env(),
    default_env = caller_env(),
    dots_env = current_env()
  )
}

#' @export
update_match <- function(
  .x,
  ...,
  .from = NULL,
  .to = NULL
) {
  obj_check_vector(.x)

  default <- .x
  ptype <- vec_ptype_finalise(vec_ptype(.x))

  case_match_with_envs(
    x = .x,
    dots = list2(...),
    from = .from,
    to = .to,
    default = default,
    ptype = ptype,
    call = current_env(),
    default_env = caller_env(),
    dots_env = current_env()
  )
}

case_match_with_envs <- function(
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
  dots <- case_formula_evaluate(
    dots = dots,
    default_env = default_env,
    dots_env = dots_env,
    error_call = call
  )

  implementation <- check_mutually_exclusive_case_match_arguments(
    dots = dots,
    from = from,
    to = to,
    from_arg = ".from",
    to_arg = ".to",
    call = call
  )

  if (implementation == "case-when") {
    result <- try_optimize_to_recode(
      lhs = dots$lhs,
      rhs = dots$rhs,
      default = default,
      default_arg = ".default",
      lhs_ptype = x,
      rhs_ptype = ptype,
      call = call
    )

    if (!is.null(result)) {
      implementation <- "recode"
      from <- result$from
      to <- result$to
      default <- result$default
    }
  }

  switch(
    implementation,
    "case-when" = {
      case_match_using_case_when(
        needles = x,
        haystacks = dots$lhs,
        values = dots$rhs,
        default = default,
        needles_arg = ".x",
        haystacks_arg = "",
        values_arg = "",
        default_arg = ".default",
        ptype = ptype,
        call = call
      )
    },
    "recode" = {
      case_match_using_recode(
        x = x,
        from = from,
        to = to,
        default = default,
        x_arg = ".x",
        from_arg = ".from",
        to_arg = ".to",
        default_arg = ".default",
        ptype = ptype,
        call = call
      )
    }
  )
}

case_match_using_case_when <- function(
  needles,
  haystacks,
  values,
  default,
  needles_arg,
  haystacks_arg,
  values_arg,
  default_arg,
  ptype,
  call
) {
  obj_check_vector(needles, arg = needles_arg, call = call)
  obj_check_list(haystacks, arg = haystacks_arg, call = call)
  list_check_all_vectors(haystacks, arg = haystacks_arg, call = call)

  haystacks <- vec_cast_common(
    !!!haystacks,
    .to = needles,
    .arg = haystacks_arg,
    .call = call
  )

  # Remove names for error handling at this point, no longer required,
  # and they get in the way with `list_unchop()`
  haystacks <- unname(haystacks)

  sizes <- list_sizes(haystacks)
  cutpoints <- cumsum(sizes)

  # We don't expect this to fail, we casted to a common type above
  haystack <- list_unchop(haystacks)

  loc <- vec_match(needles, haystack)

  # Utilize the fact that `vec_case_when()`:
  # - Uses the first `TRUE` value
  # - Treats any `NA` value as `FALSE`
  conditions <- map(cutpoints, function(cutpoint) {
    loc <= cutpoint
  })

  size <- vec_size(needles)

  vec_case_when(
    conditions = conditions,
    values = values,
    conditions_arg = "",
    values_arg = values_arg,
    default = default,
    default_arg = default_arg,
    ptype = ptype,
    size = size,
    call = call
  )
}

case_match_using_recode <- function(
  x,
  from,
  to,
  default,
  x_arg,
  from_arg,
  to_arg,
  default_arg,
  ptype,
  call
) {
  vec_recode(
    x = x,
    from = from,
    to = to,
    default = default,
    x_arg = x_arg,
    from_arg = from_arg,
    to_arg = to_arg,
    default_arg = default_arg,
    ptype = ptype,
    call = call
  )
}

check_mutually_exclusive_case_match_arguments <- function(
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
    "recode"
  } else {
    "case-when"
  }
}

# Optimizes:
#
# ```r
# case_match(
#   x,
#   1 ~ "a",
#   2 ~ "b",
#   3 ~ "c"
# )
# ```
#
# which is a common case that uses multiple `vec_in()` calls and scales
# linearly to the number of conditions provided, to:
#
# ```r
# case_match(
#   x,
#   .from = c(1, 2, 3),
#   .to = c("a", "b", "c")
# )
# ```
#
# which only uses 1 `vec_match()` call and scales independently of the number
# of conditions supplied.
#
# If any RHS is length >1, we cannot perform the optimization and require the
# full `vec_case_when()` approach.
#
# This contains a decent amount of code duplication with `vec_case_when()` to
# ensure that cast/recycle error messages are thrown early so they reference
# the formula interface rather than the `.from`/`.to` interface, but it is worth
# it for the performance improvement.
try_optimize_to_recode <- function(
  lhs,
  rhs,
  default,
  default_arg,
  lhs_ptype,
  rhs_ptype,
  call
) {
  obj_check_list(lhs, arg = "", call = call)
  obj_check_list(rhs, arg = "", call = call)
  list_check_all_vectors(lhs, arg = "", call = call)
  list_check_all_vectors(rhs, arg = "", call = call)
  if (!is_null(default)) {
    obj_check_vector(default, arg = default_arg, call = call)
  }

  if (length(rhs) == 0L) {
    # We require at least one condition before optimizing to `vec_recode()`,
    # because it has required `from` and `to` arguments (i.e. they can't be
    # `NULL`). This will fall through to a correct `vec_case_match()` error.
    return(NULL)
  }

  rhs_sizes <- list_sizes(rhs)

  if (!all(rhs_sizes == 1L)) {
    return(NULL)
  }

  # Recycle all size 1 `rhs` values up to the size of the corresponding `lhs` so
  # that the unchopped vectors are the same size
  lhs_sizes <- list_sizes(lhs)
  loc_recycle <- which(rhs_sizes != lhs_sizes)

  if (length(loc_recycle) != 0L) {
    rhs_names <- names(rhs)

    for (loc in loc_recycle) {
      rhs[[loc]] <- vec_recycle(
        x = rhs[[loc]],
        size = lhs_sizes[[loc]],
        x_arg = rhs_names[[loc]],
        call = call
      )
    }
  }

  # Cast early for best error messages, before we strip names.
  # For `rhs`, the `default` is part of the `ptype` determination, and any
  # errors related to `ptype` need to be caught here for the best error message.
  extras <- list(default)
  names(extras) <- default_arg
  everything <- c(rhs, extras)
  rhs_ptype <- vec_ptype_common(!!!everything, .to = rhs_ptype, .call = call)

  lhs <- vec_cast_common(!!!lhs, .to = lhs_ptype, .call = call)
  rhs <- vec_cast_common(!!!rhs, .to = rhs_ptype, .call = call)
  default <- vec_cast(default, rhs_ptype, x_arg = default_arg, call = call)

  # Don't want names in `list_unchop()`, which errors if there are outer names
  # and any individual vector is length >1 (which can happen with the lhs
  # values). Shouldn't error at this point anyways, all casting has been done
  # early, so we don't care about the names anyways.
  names(lhs) <- NULL
  names(rhs) <- NULL

  from <- list_unchop(
    x = lhs,
    error_arg = "",
    error_call = call
  )
  to <- list_unchop(
    x = rhs,
    error_arg = "",
    error_call = call
  )

  list(
    from = from,
    to = to,
    default = default
  )
}
