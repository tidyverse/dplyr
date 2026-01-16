#' A general vectorised `switch()`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `case_match()` is deprecated. Please use [recode_values()] and
#' [replace_values()] instead, which are more powerful, have more intuitive
#' names, and have better safety. In addition to the familiar two-sided formula
#' interface, these functions also have `from` and `to` arguments which allow
#' you to incorporate a lookup table into the recoding process.
#'
#' This function allows you to vectorise multiple [switch()] statements. Each
#' case is evaluated sequentially and the first match for each element
#' determines the corresponding value in the output vector. If no cases match,
#' the `.default` is used.
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
#' @keywords internal
#'
#' @export
#' @examples
#' # `case_match()` is deprecated and has been replaced by `recode_values()` and
#' # `replace_values()`
#'
#' x <- c("a", "b", "a", "d", "b", NA, "c", "e")
#'
#' # `recode_values()` is a 1:1 replacement for `case_match()`
#' case_match(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4
#' )
#' recode_values(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4
#' )
#'
#' # `recode_values()` has an additional `unmatched` argument to help you catch
#' # missed mappings
#' try(recode_values(
#'   x,
#'   "a" ~ 1,
#'   "b" ~ 2,
#'   "c" ~ 3,
#'   "d" ~ 4,
#'   unmatched = "error"
#' ))
#'
#' # `recode_values()` also has additional `from` and `to` arguments, which are
#' # useful when your lookup table is defined elsewhere (for example, it could
#' # be read in from a CSV file). This is very difficult to do with
#' # `case_match()`!
#' lookup <- tribble(
#'   ~from, ~to,
#'   "a", 1,
#'   "b", 2,
#'   "c", 3,
#'   "d", 4
#' )
#'
#' recode_values(x, from = lookup$from, to = lookup$to)
#'
#' # Both `case_match()` and `recode_values()` work with more than just
#' # character inputs:
#' y <- as.integer(c(1, 2, 1, 3, 1, NA, 2, 4))
#'
#' case_match(
#'   y,
#'   c(1, 3) ~ "odd",
#'   c(2, 4) ~ "even",
#'   .default = "missing"
#' )
#' recode_values(
#'   y,
#'   c(1, 3) ~ "odd",
#'   c(2, 4) ~ "even",
#'   default = "missing"
#' )
#'
#' # Or with a lookup table
#' lookup <- tribble(
#'   ~from,   ~to,
#'   c(1, 3), "odd",
#'   c(2, 4), "even"
#' )
#' recode_values(y, from = lookup$from, to = lookup$to, default = "missing")
#'
#' # `replace_values()` is a convenient way to replace selected values, leaving
#' # everything else as is. It's similar to `case_match(y, .default = y)`.
#' replace_values(y, NA ~ 0)
#' case_match(y, NA ~ 0, .default = y)
#'
#' # Notably, `replace_values()` is type stable, which means that `y` can't
#' # change types out from under you, unlike with `case_match()`!
#' typeof(y)
#' typeof(replace_values(y, NA ~ 0))
#' typeof(case_match(y, NA ~ 0, .default = y))
#'
#' # We believe that `replace_values()` better expresses intent when doing a
#' # partial replacement. Compare these two `mutate()` calls, each with the
#' # goals of:
#' # - Replace missings in `hair_color`
#' # - Replace some of the `species`
#' starwars |>
#'   mutate(
#'     hair_color = case_match(hair_color, NA ~ "unknown", .default = hair_color),
#'     species = case_match(
#'       species,
#'       "Human" ~ "Humanoid",
#'       "Droid" ~ "Robot",
#'       c("Wookiee", "Ewok") ~ "Hairy",
#'       .default = species
#'     ),
#'     .keep = "used"
#'   )
#'
#' updates <- tribble(
#'   ~from,                ~to,
#'   "Human",              "Humanoid",
#'   "Droid",              "Robot",
#'   c("Wookiee", "Ewok"), "Hairy"
#' )
#'
#' starwars |>
#'   mutate(
#'     hair_color = replace_values(hair_color, NA ~ "unknown"),
#'     species = replace_values(species, from = updates$from, to = updates$to),
#'     .keep = "used"
#'   )
case_match <- function(.x, ..., .default = NULL, .ptype = NULL) {
  lifecycle::deprecate_soft(
    when = "1.2.0",
    what = "case_match()",
    with = "recode_values()",
    id = "dplyr-case-match"
  )

  # Matching historical behavior of `case_match()`, which was to work like
  # `case_when()` and not allow empty `...`. Newer `replace_when()` and
  # `replace_values()` are a no-op for this case, but we deprecated
  # `case_match()` at that time so it never moved to the new behavior.
  allow_empty_dots <- FALSE

  args <- eval_formulas(
    ...,
    allow_empty_dots = allow_empty_dots
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

vec_case_match <- function(
  needles,
  haystacks,
  values,
  ...,
  needles_arg = "needles",
  haystacks_arg = "haystacks",
  values_arg = "values",
  default = NULL,
  default_arg = "default",
  ptype = NULL,
  call = current_env()
) {
  check_dots_empty0(...)

  obj_check_vector(needles, arg = needles_arg, call = call)
  obj_check_list(haystacks, arg = haystacks_arg, call = call)
  list_check_all_vectors(haystacks, arg = haystacks_arg, call = call)

  haystacks <- vec_cast_common(
    !!!haystacks,
    .to = needles,
    .arg = haystacks_arg,
    .call = call
  )

  conditions <- map(haystacks, vec_in, needles = needles)

  size <- vec_size(needles)

  vec_case_when(
    conditions = conditions,
    values = values,
    default = default,
    ptype = ptype,
    size = size,
    conditions_arg = "",
    values_arg = values_arg,
    default_arg = default_arg,
    error_call = call
  )
}
