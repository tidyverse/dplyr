#' A general vectorised if-else
#'
#' This function allows you to vectorise multiple [if_else()] statements. It is
#' an R equivalent of the SQL `CASE WHEN` statement. If no cases match, a
#' missing value is returned unless a `.default` is supplied.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]>
#'
#'   Pairs of inputs supplied like
#'   `condition1, value1, condition2, value2, ...`. The `condition`s determine
#'   which values match this case, the `value`s provide the value to use for
#'   this case.
#'
#'   Each `condition` input must evaluate to a logical vector. All `condition`
#'   inputs must be the same length.
#'
#'   The `value` inputs will be coerced to their common type. All `value`
#'   inputs must be length 1 or the same length as the `condition`s.
#'
#'   An `NA` in a `condition` will result in a missing value in that location
#'   of the output unless another `condition` evaluates to `TRUE` for that
#'   location.
#'
#'   If the `...` are named, those names will be utilized in any error messages.
#'
#' @param .default The default value used when all `condition`s return `FALSE`
#'   for a particular location. `.default` must be length 1 or the same length
#'   as the `condition`s. `.default` participates in the computation of the
#'   common type alongside the `value` inputs.
#'
#'   If `NULL`, the default, a missing value will be placed in the result.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of the `value` inputs.
#'
#' @param .size An optional size declaring the desired output size. If supplied,
#'   this overrides the size of the `condition` inputs.
#'
#' @return A vector with the same length as the `condition` inputs and the same
#'   type as the common type of the `value` inputs.
#'
#' @section Previous interface:
#'
#' Previously, `case_when()` used an interface based on formulas. Rather than:
#'
#' ```
#' case_when(
#'   condition1, value1,
#'   condition2, value2,
#'   .default = default
#' )
#' ```
#'
#' you used to use:
#'
#' ```
#' case_when(
#'   condition1 ~ value1,
#'   condition2 ~ value2,
#'   TRUE ~ default
#' )
#' ```
#'
#' The formula interface currently still works, but we now believe it to be
#' suboptimal and encourage you to switch to the new interface. In particular,
#' we believe that `.default` is a safer way to provide a default value, as it
#' allows us to require that all of the `condition`s have the same size. It is
#' also only applied to locations where all of the `condition`s have returned
#' `FALSE`, and doesn't apply to locations where an `NA` should have been
#' propagated through, unlike the `TRUE ~ default` approach.
#'
#' The old interface was also the only place in the tidyverse that we used
#' formulas in this way, and it had the potential to be confusing with how we
#' use formulas for modeling or for generating anonymous functions in purrr,
#' like `~ .x + 1`. We generally feel that the new interface aligns better with
#' other code that you'll write while using the tidyverse.
#'
#' @export
#' @examples
#' x <- 1:50
#'
#' # Like an if statement, the arguments are evaluated in order, so you must
#' # proceed from the most specific to the most general.
#' case_when(
#'   x %% 35 == 0, "fizz buzz",
#'   x %% 5 == 0, "fizz",
#'   x %% 7 == 0, "buzz",
#'   .default = as.character(x)
#' )
#'
#' # If none of the cases match, `NA` is used:
#' case_when(
#'   x %% 5 == 0, "fizz",
#'   x %% 7 == 0, "buzz",
#'   x %% 35 == 0, "fizz buzz"
#' )
#'
#' # An `NA` value in a logical condition gets passed through as `NA` in the
#' # output if none of the other conditions return `TRUE`
#' x[2:4] <- NA
#'
#' case_when(
#'   x %% 35 == 0, "fizz buzz",
#'   x %% 5 == 0, "fizz",
#'   x %% 7 == 0, "buzz",
#'   .default = as.character(x)
#' )
#'
#' # Use `is.na()` to handle the missing values if you know where they are
#' # coming from
#' case_when(
#'   x %% 35 == 0, "fizz buzz",
#'   x %% 5 == 0, "fizz",
#'   x %% 7 == 0, "buzz",
#'   is.na(x), "nope",
#'   .default = as.character(x)
#' )
#'
#' # case_when() evaluates all value expressions, and then constructs its
#' # result by extracting the selected (via the condition expressions) parts.
#' # In particular `NaN`s are produced in this case:
#' y <- seq(-2, 2, by = .5)
#' case_when(
#'   y >= 0, sqrt(y),
#'   .default = y
#' )
#'
#' # `case_when()` is particularly useful inside `mutate()` when you want to
#' # create a new variable that relies on a complex combination of existing
#' # variables
#' starwars %>%
#'   select(name:mass, gender, species) %>%
#'   mutate(
#'     type = case_when(
#'       height > 200 | mass > 200, "large",
#'       species == "Droid", "robot",
#'       .default = "other"
#'     )
#'   )
#'
#'
#' # `case_when()` is not a tidy eval function. If you'd like to reuse
#' # the same patterns, extract the `case_when()` call in a normal
#' # function:
#' case_character_type <- function(height, mass, species) {
#'   case_when(
#'     height > 200 | mass > 200, "large",
#'     species == "Droid", "robot",
#'     .default = "other"
#'   )
#' }
#'
#' case_character_type(150, 250, "Droid")
#' case_character_type(150, 150, "Droid")
#'
#' # Such functions can be used inside `mutate()` as well:
#' starwars %>%
#'   mutate(type = case_character_type(height, mass, species)) %>%
#'   pull(type)
case_when <- function(...,
                      .default = NULL,
                      .ptype = NULL,
                      .size = NULL) {
  args <- list2(...)

  any_quosures <- any(map_lgl(args, is_quosure))
  any_formulas <- any(map_lgl(args, is_formula))

  if (any_quosures || any_formulas) {
    if (!is.null(.default)) {
      abort("`.default` can only be used with the new interface.")
    }
    if (!is.null(.ptype)) {
      abort("`.ptype` can only be used with the new interface.")
    }
    if (!is.null(.size)) {
      abort("`.size` can only be used with the new interface.")
    }

    default_env <- caller_env()
    dots_env <- current_env()
    error_call <- caller_env()

    # Handle the old interface
    args <- case_when_formula_evaluate(
      args = args,
      default_env = default_env,
      dots_env = dots_env,
      error_call = error_call
    )
  }

  vec_case_when(
    !!!args,
    .default = .default,
    .ptype = .ptype,
    .size = .size
  )
}

# ------------------------------------------------------------------------------
# Backwards compatibility

case_when_formula_evaluate <- function(args,
                                       default_env,
                                       dots_env,
                                       error_call) {
  # `case_when()` used to compact `NULL`s, but the new interface doesn't
  # because of how the inputs are supplied in pairs
  args <- compact_null(args)
  n <- length(args)

  where <- vector("list", n)
  value <- vector("list", n)

  quos_pairs <- map2(
    .x = args,
    .y = seq_along(args),
    .f = validate_formula,
    default_env = default_env,
    dots_env = dots_env,
    error_call = error_call
  )

  for (i in seq_len(n)) {
    pair <- quos_pairs[[i]]
    where[[i]] <- eval_tidy(pair$lhs, env = default_env)
    value[[i]] <- eval_tidy(pair$rhs, env = default_env)
  }

  # Add the formula call as the name for both `where` and `value`.
  # These names also get passed on to `vec_case_when()`.
  names <- parse_named_call(args)
  names(where) <- names
  names(value) <- names

  # `case_when()` used to find the common size of ALL of its inputs.
  # This is what allowed `TRUE ~` to work.
  size <- vec_size_common(!!!where, !!!value, .call = error_call)

  where <- vec_recycle_common(!!!where, .size = size)
  value <- vec_recycle_common(!!!value, .size = size)

  args <- vec_interleave(where, value)

  args
}

validate_formula <- function(x,
                             i,
                             default_env,
                             dots_env,
                             error_call) {
  if (is_quosure(x)) {
    # We specially handle quosures, assuming they hold formulas.
    # The new interface doesn't allow quosures.
    default_env <- quo_get_env(x)
    x <- quo_get_expr(x)
  }

  if (!is_formula(x)) {
    arg <- substitute(...(), dots_env)[[1]]
    abort_case_when_formula(arg, i, x, error_call = error_call)
  }
  if (is_null(f_lhs(x))) {
    abort("Formulas must be two-sided.", call = error_call)
  }

  # Formula might be unevaluated, e.g. if it's been quosured
  env <- f_env(x) %||% default_env

  list(
    lhs = new_quosure(f_lhs(x), env),
    rhs = new_quosure(f_rhs(x), env)
  )
}

abort_case_when_formula <- function(arg, i, obj, error_call = caller_env()) {
  deparsed <- fmt_obj1(deparse_trunc(arg))
  type <- friendly_type_of(obj)
  msg <- glue("Case {i} ({deparsed}) must be a two-sided formula, not {type}.")
  abort(msg, call = error_call)
}
