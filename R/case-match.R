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
case_match <- function(.x,
                       ...,
                       .default = NULL,
                       .ptype = NULL) {
  args <- list2(...)

  args <- case_formula_evaluate(
    args = args,
    default_env = caller_env(),
    dots_env = current_env(),
    error_call = current_env()
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
    call = current_env()
  )
}
