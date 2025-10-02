#' A general vectorised if-else
#'
#' @description
#' This function allows you to vectorise multiple [if_else()] statements. Each
#' case is evaluated sequentially and the first match for each element
#' determines the corresponding value in the output vector. If no cases match,
#' the `.default` is used as a final "else" statment.
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
#'   For historical reasons, all LHS inputs will be recycled to their common
#'   size. That said, we encourage all LHS inputs to be the same size, which you
#'   can optionally enforce with `.size`. All RHS inputs will be recycled to the
#'   common size of the LHS inputs.
#'
#'   `NULL` inputs are ignored.
#'
#' @param .default The value used when all of the LHS inputs return either
#'   `FALSE` or `NA`.
#'
#'   `.default` must be size 1 or the same size as the common size computed
#'   from the LHS inputs.
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
#'   this overrides the common size computed from the LHS inputs.
#'
#' @return A vector
#'
#'   - The size of the vector is the common size of the LHS inputs, or `.size`.
#'   - The type of the vector is the common type of the RHS inputs, or `.ptype`.
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
#' starwars |>
#'   select(name:mass, gender, species) |>
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
#' starwars |>
#'   mutate(type = case_character_type(height, mass, species)) |>
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
#' starwars |>
#'   mutate(type = case_character_type(height, mass, species, robots = FALSE)) |>
#'   pull(type)
case_when <- function(..., .default = NULL, .ptype = NULL, .size = NULL) {
  args <- list2(...)

  args <- case_formula_evaluate(
    args = args,
    default_env = caller_env(),
    dots_env = current_env(),
    error_call = current_env()
  )

  conditions <- args$lhs
  values <- args$rhs

  .size <- case_when_size_common(
    conditions = conditions,
    values = values,
    size = .size
  )

  # Only recycle `conditions`. Expect that `vec_case_when()` requires all
  # `conditions` to be the same size, but can efficiently recycle `values` at
  # the C level without extra allocations.
  conditions <- vec_recycle_common(!!!conditions, .size = .size)

  vec_case_when(
    conditions = conditions,
    values = values,
    default = .default,
    ptype = .ptype,
    size = .size,
    conditions_arg = "",
    values_arg = "",
    default_arg = ".default",
    error_call = current_env()
  )
}

# Size common computation for `case_when()`
#
# `case_when()`'s formula interface historically finds the common size of ALL
# inputs. This is not good, ideally it would force all LHS inputs to have the
# same size (with no recycling), and then recycle all RHS inputs to that size
# inferred from the LHS. That is how `vec_case_when()` works.
#
# We can't change this easily for two reasons:
#
# - `TRUE ~` must continue to work for legacy reasons, so at the very least all
#   LHS inputs must be recycled against each other. We are okay with this.
#
# - Many packages (60+) use `case_when()` with scalar LHSs but vector RHSs,
#   requiring that all inputs by recycled against each other. This usage should
#   be replaced with a series of if statements. This is a highly inefficient use
#   of `case_when()` because each scalar LHS has to be recycled to the size
#   determined from the RHS, which is a big waste of memory and time. This
#   behavior can also allow real bugs to slip through silently (#7082), which is
#   bad. To combat this case, we specially detect this and throw a deprecation
#   warning.
#
# There are four cases to consider:
#
# 1. `size_conditions == 1, size_values == 1`
#
#    Fine, use size 1
#
# 2. `size_conditions == 1, size_values != 1`
#
#    Use `size_values` for historical reasons, but warn against this. This is
#    people doing off-label usage of `case_when()` when they should be using a
#    series of if statements.
#
# 3. `size_conditions != 1, size_values == 1`
#
#    Fine, use `size_conditions`
#
# 4. `size_conditions != 1, size_values != 1`
#
#    If `size_conditions == size_values`, good to go, else throw an error by
#    recalling `vec_size_common()` with all inputs.
case_when_size_common <- function(
  conditions,
  values,
  size,
  ...,
  user_env = caller_env(2),
  error_call = caller_env()
) {
  # These error if there are any size incompatibilites within LHS and RHS inputs,
  # but not across LHS and RHS inputs
  size_conditions <- vec_size_common(
    !!!conditions,
    .size = size,
    .call = error_call
  )
  size_values <- vec_size_common(
    !!!values,
    .size = size,
    .call = error_call
  )

  if (size_conditions == 1L && size_values == 1L) {
    return(1L)
  }

  if (size_conditions == 1L && size_values != 1L) {
    warn_case_when_scalar_lhs_vector_rhs(
      env = error_call,
      user_env = user_env
    )
    return(size_values)
  }

  if (size_conditions != 1L && size_values == 1L) {
    return(size_conditions)
  }

  if (size_conditions != 1L && size_values != 1L) {
    if (size_conditions == size_values) {
      return(size_conditions)
    }

    # Errors
    vec_size_common(
      !!!conditions,
      !!!values,
      .size = size,
      .call = error_call
    )

    abort("`vec_size_common()` should have errored.", .internal = TRUE)
  }

  abort("All cases should have been covered.", .internal = TRUE)
}

warn_case_when_scalar_lhs_vector_rhs <- function(
  env,
  user_env
) {
  what <- I(
    "Calling `case_when()` with size 1 LHS inputs and size >1 RHS inputs"
  )

  details <- no_cli_wrapping(paste(
    sep = "\n",
    "This `case_when()` statement can result in subtle silent bugs and is very inefficient.",
    "",
    "  Please use a series of if statements instead:",
    "",
    "  ```",
    "  # Previously",
    "  case_when(scalar_lhs1 ~ rhs1, scalar_lhs2 ~ rhs2, .default = default)",
    "",
    "  # Now",
    "  if (scalar_lhs1) {",
    "    rhs1",
    "  } else if (scalar_lhs2) {",
    "    rhs2",
    "  } else {",
    "    default",
    "  }",
    "  ```"
  ))

  lifecycle::deprecate_soft(
    when = "1.2.0",
    what = what,
    details = details,
    env = env,
    user_env = user_env
  )
}

# Suppress cli wrapping https://cli.r-lib.org/reference/inline-markup.html#wrapping
no_cli_wrapping <- function(x) {
  x <- gsub(" ", "\u00a0", x, fixed = TRUE)
  x <- gsub("\n", "\f", x, fixed = TRUE)
  x
}

case_formula_evaluate <- function(args, default_env, dots_env, error_call) {
  # `case_when()`'s formula interface compacts `NULL`s
  args <- compact_null(args)
  n_args <- length(args)
  seq_args <- seq_len(n_args)

  if (n_args == 0L) {
    abort("At least one condition must be supplied.", call = error_call)
  }

  pairs <- map2(
    .x = args,
    .y = seq_args,
    .f = function(x, i) {
      validate_and_split_formula(
        x = x,
        i = i,
        default_env = default_env,
        dots_env = dots_env,
        error_call = error_call
      )
    }
  )

  lhs <- vector("list", n_args)
  rhs <- vector("list", n_args)

  env_error_info <- new_environment()

  # Using 1 call to `withCallingHandlers()` that wraps all `eval_tidy()`
  # evaluations to avoid repeated handler setup (#6674)
  withCallingHandlers(
    for (i in seq_args) {
      env_error_info[["i"]] <- i
      pair <- pairs[[i]]

      env_error_info[["side"]] <- "left"
      elt_lhs <- eval_tidy(pair$lhs, env = default_env)

      env_error_info[["side"]] <- "right"
      elt_rhs <- eval_tidy(pair$rhs, env = default_env)

      if (!is.null(elt_lhs)) {
        lhs[[i]] <- elt_lhs
      }
      if (!is.null(elt_rhs)) {
        rhs[[i]] <- elt_rhs
      }
    },
    error = function(cnd) {
      message <- glue::glue_data(
        env_error_info,
        "Failed to evaluate the {side}-hand side of formula {i}."
      )
      abort(message, parent = cnd, call = error_call)
    }
  )

  # TODO: Ideally we'd name the lhs/rhs values with their `as_label()`-ed
  # expressions. But `as_label()` is much too slow for that to be useful in
  # a grouped `mutate()`. We need a way to add ALTREP lazy names that only get
  # materialized on demand (i.e. on error). Until then, we fall back to the
  # positional names (like `..1` or `..3`) with info about left/right (#6674).
  #
  # # Add the expressions as names for `lhs` and `rhs` for nice errors.
  # # These names also get passed on to the underlying vctrs backend.
  # lhs_names <- map(quos_pairs, function(pair) pair$lhs)
  # lhs_names <- map_chr(lhs_names, as_label)
  # names(lhs) <- lhs_names
  #
  # rhs_names <- map(quos_pairs, function(pair) pair$rhs)
  # rhs_names <- map_chr(rhs_names, as_label)
  # names(rhs) <- rhs_names
  if (n_args > 0L) {
    names(lhs) <- paste0("..", seq_args, " (left)")
    names(rhs) <- paste0("..", seq_args, " (right)")
  }

  list(
    lhs = lhs,
    rhs = rhs
  )
}

validate_and_split_formula <- function(
  x,
  i,
  default_env,
  dots_env,
  error_call
) {
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
