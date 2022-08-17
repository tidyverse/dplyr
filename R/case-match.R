#' A general vectorised switch
#'
#' This function is a variant of [case_when()] that vectorises the idea behind
#' [switch()]. Use this to repeatedly recode one or more values in `.x` to a new
#' value. It is an R equivalent of the SQL "simple" `CASE WHEN` statement that
#' matches using values rather than logical expressions. If no cases match, a
#' missing value is returned unless a `.default` is supplied.
#'
#' @param .x A vector to match against.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas. The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   The LHS inputs must evaluate to vectors with the same type as `.x`. There
#'   are no restrictions on the size of the LHS inputs, meaning that you can
#'   map multiple values in `.x` to the same RHS value. If a value in `.x` is
#'   matched to multiple LHS inputs, the first match is used.
#'
#'   The RHS inputs will be coerced to their common type. Each RHS input will be
#'   [recycled][vctrs::vector_recycling_rules] to the size of `.x`.
#'
#'   `NULL` inputs are ignored.
#'
#' @param .default The value used when values in `.x` aren't matched by any of
#'   the LHS inputs.
#'
#'   `.default` is [recycled][vctrs::vector_recycling_rules] to the size of
#'   `.x`.
#'
#'   `.default` participates in the computation of the common type with the RHS
#'   inputs.
#'
#'   If `NULL`, the default, a missing value will be used.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of the RHS inputs.
#'
#' @return
#' A vector with the same size as `.x` and the same type as the common type of
#' the RHS inputs in `...`.
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
#' # `case_match()` isn't limited to character input,
#' # you can match against any vector type
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
case_match <- function(.x,
                       ...,
                       .default = NULL,
                       .ptype = NULL) {
  args <- list2(...)

  default_env <- caller_env()
  dots_env <- current_env()
  error_call <- current_env()

  args <- case_formula_evaluate(
    args = args,
    default_env = default_env,
    dots_env = dots_env,
    error_call = error_call
  )

  haystacks <- args$lhs
  values <- args$rhs

  vec_case_match(
    needles = .x,
    haystacks = haystacks,
    values = values,
    needles_arg = ".x",
    haystacks_arg = "",
    values_arg = "",
    default = .default,
    default_arg = ".default",
    ptype = .ptype,
    call = error_call
  )
}
