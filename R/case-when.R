#' A general vectorised if-else
#'
#' @description
#' This function allows you to vectorise multiple [if_else()] statements. Each
#' case is evaluated sequentially and the first match for each element
#' determines the corresponding value in the output vector. If no cases match,
#' the `.default` is used.
#'
#' `case_when()` is an R equivalent of the SQL "searched" `CASE WHEN` statement.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas. The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   The LHS inputs must evaluate to logical vectors.
#'
#'   The RHS inputs will be coerced to their common type.
#'
#'   All inputs will be recycled to their common size. That said, we encourage
#'   all LHS inputs to be the same size. Recycling is mainly useful for RHS
#'   inputs, where you might supply a size 1 input that will be recycled to the
#'   size of the LHS inputs.
#'
#'   `NULL` inputs are ignored.
#'
#' @param .default The value used when all of the LHS inputs return either
#'   `FALSE` or `NA`.
#'
#'   `.default` must be size 1 or the same size as the common size computed
#'   from `...`.
#'
#'   `.default` participates in the computation of the common type with the RHS
#'   inputs.
#'
#'   `NA` values in the LHS conditions are treated like `FALSE`, meaning that
#'   the result at those locations will be assigned the `.default` value. To
#'   handle missing values in the conditions differently, you must explicitly
#'   catch them with another condition before they fall through to the
#'   `.default`. This typically involves some variation of `is.na(x) ~ value`
#'   tailored to your usage of `case_when()`.
#'
#'   If `NULL`, the default, a missing value will be used.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of the RHS inputs.
#'
#' @param .size An optional size declaring the desired output size. If supplied,
#'   this overrides the common size computed from `...`.
#'
#' @return A vector with the same size as the common size computed from the
#'   inputs in `...` and the same type as the common type of the RHS inputs
#'   in `...`.
#'
#' @seealso [case_match()]
#'
#' @export
#' @examples
#' x <- 1:70
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   .default = as.character(x)
#' )
#'
#' # Like an if statement, the arguments are evaluated in order, so you must
#' # proceed from the most specific to the most general. This won't work:
#' case_when(
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz",
#'   .default = as.character(x)
#' )
#'
#' # If none of the cases match and no `.default` is supplied, NA is used:
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#' )
#'
#' # Note that `NA` values on the LHS are treated like `FALSE` and will be
#' # assigned the `.default` value. You must handle them explicitly if you
#' # want to use a different value. The exact way to handle missing values is
#' # dependent on the set of LHS conditions you use.
#' x[2:4] <- NA_real_
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   is.na(x) ~ "nope",
#'   .default = as.character(x)
#' )
#'
#' # `case_when()` evaluates all RHS expressions, and then constructs its
#' # result by extracting the selected (via the LHS expressions) parts.
#' # In particular `NaN`s are produced in this case:
#' y <- seq(-2, 2, by = .5)
#' case_when(
#'   y >= 0 ~ sqrt(y),
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
#'       height > 200 | mass > 200 ~ "large",
#'       species == "Droid" ~ "robot",
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
#'     height > 200 | mass > 200 ~ "large",
#'     species == "Droid" ~ "robot",
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
#'
#' # `case_when()` ignores `NULL` inputs. This is useful when you'd
#' # like to use a pattern only under certain conditions. Here we'll
#' # take advantage of the fact that `if` returns `NULL` when there is
#' # no `else` clause:
#' case_character_type <- function(height, mass, species, robots = TRUE) {
#'   case_when(
#'     height > 200 | mass > 200 ~ "large",
#'     if (robots) species == "Droid" ~ "robot",
#'     .default = "other"
#'   )
#' }
#'
#' starwars %>%
#'   mutate(type = case_character_type(height, mass, species, robots = FALSE)) %>%
#'   pull(type)
case_when <- function(...,
                      .default = NULL,
                      .ptype = NULL,
                      .size = NULL) {
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

  conditions <- args$lhs
  values <- args$rhs

  # `case_when()`'s formula interface finds the common size of ALL of its inputs.
  # This is what allows `TRUE ~` to work.
  .size <- vec_size_common(!!!conditions, !!!values, .size = .size, .call = error_call)

  conditions <- vec_recycle_common(!!!conditions, .size = .size, .call = error_call)
  values <- vec_recycle_common(!!!values, .size = .size, .call = error_call)

  vec_case_when(
    conditions = conditions,
    values = values,
    conditions_arg = "",
    values_arg = "",
    default = .default,
    default_arg = ".default",
    ptype = .ptype,
    size = .size,
    call = error_call
  )
}

case_formula_evaluate <- function(args,
                                  default_env,
                                  dots_env,
                                  error_call) {
  # `case_when()`'s formula interface compacts `NULL`s
  args <- compact_null(args)
  n_args <- length(args)

  lhs <- vector("list", n_args)
  rhs <- vector("list", n_args)

  quos_pairs <- map2(
    .x = args,
    .y = seq_len(n_args),
    .f = validate_and_split_formula,
    default_env = default_env,
    dots_env = dots_env,
    error_call = error_call
  )

  for (i in seq_len(n_args)) {
    pair <- quos_pairs[[i]]

    lhs_elt <- with_case_errors(
      eval_tidy(pair$lhs, env = default_env),
      side = "left",
      i = i,
      error_call = error_call
    )
    rhs_elt <- with_case_errors(
      eval_tidy(pair$rhs, env = default_env),
      side = "right",
      i = i,
      error_call = error_call
    )

    if (!is.null(lhs_elt)) {
      lhs[[i]] <- lhs_elt
    }
    if (!is.null(rhs_elt)) {
      rhs[[i]] <- rhs_elt
    }
  }

  # Add the expressions as names for `lhs` and `rhs` for nice errors.
  # These names also get passed on to the underlying vctrs backend.
  lhs_names <- map(quos_pairs, function(pair) pair$lhs)
  lhs_names <- map_chr(lhs_names, as_label)
  names(lhs) <- lhs_names

  rhs_names <- map(quos_pairs, function(pair) pair$rhs)
  rhs_names <- map_chr(rhs_names, as_label)
  names(rhs) <- rhs_names

  list(
    lhs = lhs,
    rhs = rhs
  )
}

validate_and_split_formula <- function(x,
                                       i,
                                       default_env,
                                       dots_env,
                                       error_call) {
  if (is_quosure(x)) {
    # We specially handle quosures, assuming they hold formulas
    default_env <- quo_get_env(x)
    x <- quo_get_expr(x)
  }

  if (!is_formula(x, lhs = TRUE)) {
    arg <- substitute(...(), dots_env)[[i]]
    arg <- glue::backtick(as_label(arg))

    if (is_formula(x)) {
      type <- "a two-sided formula"
    } else {
      type <- glue("a two-sided formula, not {obj_type_friendly(x)}")
    }

    message <- glue("Case {i} ({arg}) must be {type}.")
    abort(message, call = error_call)
  }

  # Formula might be unevaluated, e.g. if it's been quosured
  env <- f_env(x) %||% default_env

  list(
    lhs = new_quosure(f_lhs(x), env),
    rhs = new_quosure(f_rhs(x), env)
  )
}

with_case_errors <- function(expr, side, i, error_call) {
  try_fetch(
    expr,
    error = function(cnd) {
      message <- glue("Failed to evaluate the {side}-hand side of formula {i}.")
      abort(message, parent = cnd, call = error_call)
    }
  )
}
