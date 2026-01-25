#' A general vectorised if-else
#'
#' @description
#' `case_when()` and `replace_when()` are two forms of vectorized [if_else()].
#' They work by evaluating each case sequentially and using the first match for
#' each element to determine the corresponding value in the output vector.
#'
#' - Use `case_when()` when creating an entirely new vector.
#'
#' - Use `replace_when()` when partially updating an existing vector.
#'
#' If you are just replacing a few values within an existing vector, then
#' `replace_when()` is always a better choice because it is type stable, size
#' stable, pipes better, and better expresses intent.
#'
#' A major difference between the two functions is what happens when no cases
#' match:
#'
#' - `case_when()` falls through to a `.default` as a final "else" statement.
#'
#' - `replace_when()` retains the original values from `x`.
#'
#' See `vignette("recoding-replacing")` for more examples.
#'
#' @param x A vector.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas. The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   For `case_when()`:
#'
#'   - The LHS inputs must be logical vectors. For backwards compatibility,
#'     scalars are [recycled][vctrs::theory-faq-recycling], but we no longer
#'     recommend supplying scalars.
#'
#'   - The RHS inputs will be [cast][vctrs::theory-faq-coercion] to their common
#'     type, and will be [recycled][vctrs::theory-faq-recycling] to the common
#'     size of the LHS inputs.
#'
#'   For `replace_when()`:
#'
#'   - The LHS inputs must be logical vectors the same size as `x`.
#'
#'   - The RHS inputs will be [cast][vctrs::theory-faq-coercion] to the type of
#'     `x` and [recycled][vctrs::theory-faq-recycling] to the size of `x`.
#'
#'   `NULL` inputs are ignored.
#'
#' @param .default The value used when all of the LHS inputs return either
#'   `FALSE` or `NA`.
#'
#'   - If `NULL`, the default, a missing value will be used.
#'
#'   - If provided, `.default` will follow the same type and size rules as the
#'     RHS inputs.
#'
#'   `NA` values in the LHS conditions are treated like `FALSE`, meaning that
#'   the result at those locations will be assigned the `.default` value. To
#'   handle missing values in the conditions differently, you must explicitly
#'   catch them with another condition before they fall through to the
#'   `.default`. This typically involves some variation of `is.na(x) ~ value`
#'   tailored to your usage of `case_when()`.
#'
#' @param .unmatched Handling of unmatched locations.
#'
#'   One of:
#'
#'   - `"default"` to use `.default` in unmatched locations.
#'
#'   - `"error"` to error when there are unmatched locations.
#'
#' @param .ptype An optional prototype declaring the desired output type. If
#'   supplied, this overrides the common type of the RHS inputs.
#'
#' @param .size An optional size declaring the desired output size. If supplied,
#'   this overrides the common size computed from the LHS inputs.
#'
#' @returns
#' For `case_when()`, a new vector where the size is the common size of the LHS
#' inputs, the type is the common type of the RHS inputs, and the names
#' correspond to the names of the RHS elements used in the result.
#'
#' For `replace_when()`, an updated version of `x`, with the same size, type,
#' and names as `x`.
#'
#' @seealso [recode_values()], [vctrs::vec_case_when()]
#'
#' @name case-and-replace-when
#'
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
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz",
#'   .default = as.character(x)
#' )
#'
#' # If none of the cases match and no `.default` is supplied, NA is used:
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz"
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
#' # `case_when()` is not a replacement for basic if/else control flow. When
#' # you have a single scalar condition, using if/else is faster, simpler to
#' # reason about, and is lazy on the branch that isn't run. For example, this
#' # seems to work:
#' x <- "value"
#' case_when(is.character(x) ~ x, .default = "not-a-character")
#'
#' # Until `x` is a non-character type
#' x <- 1
#' try(case_when(is.character(x) ~ x, .default = "not-a-character"))
#'
#' # Instead, you should use if/else
#' if (is.character(x)) {
#'   y <- x
#' } else {
#'   y <- "not-a-character"
#' }
#' y
#'
#' # If you believe that you've covered every possible case, then set
#' # `.unmatched = "error"` rather than supplying a `.default`. This adds an
#' # extra layer of safety to `case_when()` and is particularly useful when you
#' # have a series of complex expressions!
#' set.seed(123)
#' x <- sample(50)
#'
#' # Oops, we forgot to handle `50`
#' try(case_when(
#'   x < 10 ~ "ten",
#'   x < 20 ~ "twenty",
#'   x < 30 ~ "thirty",
#'   x < 40 ~ "forty",
#'   x < 50 ~ "fifty",
#'   .unmatched = "error"
#' ))
#'
#' case_when(
#'   x < 10 ~ "ten",
#'   x < 20 ~ "twenty",
#'   x < 30 ~ "thirty",
#'   x < 40 ~ "forty",
#'   x <= 50 ~ "fifty",
#'   .unmatched = "error"
#' )
#'
#' # Note that `NA` is considered unmatched and must be handled with its own
#' # explicit case, even if that case just propagates the missing value!
#' x[c(2, 5)] <- NA
#'
#' case_when(
#'   x < 10 ~ "ten",
#'   x < 20 ~ "twenty",
#'   x < 30 ~ "thirty",
#'   x < 40 ~ "forty",
#'   x <= 50 ~ "fifty",
#'   is.na(x) ~ NA,
#'   .unmatched = "error"
#' )
#'
#' # `replace_when()` is useful when you're updating an existing vector,
#' # rather than creating an entirely new one. Note the so-far unused "puppy"
#' # factor level:
#' pets <- tibble(
#'   name = c("Max", "Bella", "Chuck", "Luna", "Cooper"),
#'   type = factor(
#'     c("dog", "dog", "cat", "dog", "cat"),
#'     levels = c("dog", "cat", "puppy")
#'   ),
#'   age = c(1, 3, 5, 2, 4)
#' )
#'
#' # We can replace some values with `"puppy"` based on arbitrary conditions.
#' # Even though we are using a character `"puppy"` value, `replace_when()` will
#' # automatically cast it to the factor type of `type` for us.
#' pets |>
#'   mutate(
#'     type = replace_when(type, type == "dog" & age <= 2 ~ "puppy")
#'   )
#'
#' # Compare that with this `case_when()` call, which loses the factor class.
#' # It's always better to use `replace_when()` when updating a few values in
#' # an existing vector!
#' pets |>
#'   mutate(
#'     type = case_when(type == "dog" & age <= 2 ~ "puppy", .default = type)
#'   )
#'
#' # `case_when()` and `replace_when()` evaluate all RHS expressions, and then
#' # construct their result by extracting the selected (via the LHS expressions)
#' # parts. For example, `NaN`s are produced here because `sqrt(y)` is evaluated
#' # on all of `y`, not just where `y >= 0`.
#' y <- seq(-2, 2, by = .5)
#' replace_when(y, y >= 0 ~ sqrt(y))
#'
#' # These functions are particularly useful inside `mutate()` when you want to
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
#' # `case_when()` is not a tidy eval function. If you'd like to reuse
#' # the same patterns, extract the `case_when()` call into a normal
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
#'
#' # `replace_when()` can also be used in combination with `pick()` to
#' # conditionally mutate rows within multiple columns using a single condition.
#' # Here `replace_when()` returns a data frame with new `species` and `name`
#' # columns, which `mutate()` then automatically unpacks.
#' starwars |>
#'   select(homeworld, species, name) |>
#'   mutate(replace_when(
#'     pick(species, name),
#'     homeworld == "Tatooine" ~ tibble(
#'       species = "Tatooinese",
#'       name = paste(name, "(Tatooine)")
#'     )
#'   ))
NULL

#' @rdname case-and-replace-when
#' @export
case_when <- function(
  ...,
  .default = NULL,
  .unmatched = "default",
  .ptype = NULL,
  .size = NULL
) {
  args <- eval_formulas(..., allow_empty_dots = FALSE)
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
    unmatched = .unmatched,
    ptype = .ptype,
    size = .size,
    conditions_arg = "",
    values_arg = "",
    default_arg = ".default",
    error_call = current_env()
  )
}

#' @rdname case-and-replace-when
#' @export
replace_when <- function(x, ...) {
  check_dots_unnamed()

  args <- eval_formulas(..., allow_empty_dots = TRUE)
  conditions <- args$lhs
  values <- args$rhs

  vec_replace_when(
    x = x,
    conditions = conditions,
    values = values,
    x_arg = "x",
    conditions_arg = "",
    values_arg = "",
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
    .absent = -1L,
    .call = error_call
  )
  size_values <- vec_size_common(
    !!!values,
    .size = size,
    .absent = -1L,
    .call = error_call
  )

  # Annoying handling of the edge case of all `NULL` elements in the formulas.
  # `vec_case_when()` will error on these and require vector inputs, but we have
  # to get to it first (#7794).
  if (size_conditions == -1L || size_values == -1L) {
    if (size_conditions != -1L) {
      # All `values` are `NULL`, but at least 1 `conditions` is non-`NULL`
      return(size_conditions)
    } else if (size_values != -1L) {
      # All `conditions` are `NULL`, but at least 1 `values` is non-`NULL`
      return(size_values)
    } else {
      # All `conditions` and `values` are `NULL`
      return(0L)
    }
  }

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
    user_env = user_env,
    id = "dplyr-case-when-scalar-lhs-vector-rhs"
  )
}

# Suppress cli wrapping https://cli.r-lib.org/reference/inline-markup.html#wrapping
no_cli_wrapping <- function(x) {
  x <- gsub(" ", "\u00a0", x, fixed = TRUE)
  x <- gsub("\n", "\f", x, fixed = TRUE)
  x
}

eval_formulas <- function(
  ...,
  allow_empty_dots,
  dots_env = current_env(),
  user_env = caller_env(2),
  error_call = caller_env()
) {
  dots <- list2(...)

  # Store index computed before dropping `NULL` so error indices are correct
  indices <- seq_along(dots)

  # Drop `NULL`s
  if (vec_any_missing(dots)) {
    not_missing <- !vec_detect_missing(dots)
    dots <- vec_slice(dots, not_missing)
    indices <- vec_slice(indices, not_missing)
  }

  dots_size <- length(dots)
  dots_seq <- seq_len(dots_size)

  if (!allow_empty_dots && dots_size == 0L) {
    abort("`...` can't be empty.", call = error_call)
  }

  pairs <- map2(
    .x = dots,
    .y = indices,
    .f = function(dot, index) {
      validate_and_split_formula(
        dot = dot,
        index = index,
        dots_env = dots_env,
        user_env = user_env,
        error_call = error_call
      )
    }
  )

  lhs <- vector("list", dots_size)
  rhs <- vector("list", dots_size)

  env_error_info <- new_environment()

  # Using 1 call to `withCallingHandlers()` that wraps all `eval_tidy()`
  # evaluations to avoid repeated handler setup (#6674)
  withCallingHandlers(
    for (i in dots_seq) {
      env_error_info[["index"]] <- indices[[i]]
      pair <- pairs[[i]]

      env_error_info[["side"]] <- "left"
      elt_lhs <- eval_tidy(pair$lhs, env = user_env)

      env_error_info[["side"]] <- "right"
      elt_rhs <- eval_tidy(pair$rhs, env = user_env)

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
        "Failed to evaluate the {side}-hand side of formula {index}."
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
  if (dots_size > 0L) {
    names(lhs) <- paste0("..", indices, " (left)")
    names(rhs) <- paste0("..", indices, " (right)")
  }

  list(
    lhs = lhs,
    rhs = rhs
  )
}

validate_and_split_formula <- function(
  dot,
  index,
  dots_env,
  user_env,
  error_call
) {
  if (is_quosure(dot)) {
    # We specially handle quosures, assuming they hold formulas
    user_env <- quo_get_env(dot)
    dot <- quo_get_expr(dot)
  }

  if (!is_formula(dot, lhs = TRUE)) {
    arg <- substitute(...(), dots_env)[[index]]
    arg <- glue::backtick(as_label(arg))

    if (is_formula(dot)) {
      type <- "a two-sided formula, not a one-sided formula"
    } else {
      type <- glue("a two-sided formula, not {obj_type_friendly(dot)}")
    }

    message <- glue("Case {index} ({arg}) must be {type}.")
    abort(message, call = error_call)
  }

  # Formula might be unevaluated, e.g. if it's been quosured
  env <- f_env(dot) %||% user_env

  list(
    lhs = new_quosure(f_lhs(dot), env),
    rhs = new_quosure(f_rhs(dot), env)
  )
}
