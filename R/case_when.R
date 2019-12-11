#' A general vectorised if
#'
#' This function allows you to vectorise multiple [if_else()]
#' statements. It is an R equivalent of the SQL `CASE WHEN` statement.
#' If no cases match, `NA` is returned.
#'
#' @param ... A sequence of two-sided formulas. The left hand side (LHS)
#'   determines which values match this case. The right hand side (RHS)
#'   provides the replacement value.
#'
#'   The LHS must evaluate to a logical vector. The RHS does not need to be
#'   logical, but all RHSs must evaluate to the same type of vector.
#'
#'   Both LHS and RHS may have the same length of either 1 or `n`. The
#'   value of `n` must be consistent across all cases. The case of
#'   `n == 0` is treated as a variant of `n != 1`.
#'
#'   `NULL` inputs are ignored.
#'
#'   These dots support [tidy dots][rlang::list2] features. In
#'   particular, if your patterns are stored in a list, you can
#'   splice that in with `!!!`.
#' @export
#' @return A vector of length 1 or `n`, matching the length of the logical
#'   input or output vectors, with the type (and attributes) of the first
#'   RHS. Inconsistent lengths or types will generate an error.
#' @examples
#' x <- 1:50
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#'
#' # Like an if statement, the arguments are evaluated in order, so you must
#' # proceed from the most specific to the most general. This won't work:
#' case_when(
#'   TRUE ~ as.character(x),
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz"
#' )
#'
#' # If none of the cases match, NA is used:
#' case_when(
#'   x %%  5 == 0 ~ "fizz",
#'   x %%  7 == 0 ~ "buzz",
#'   x %% 35 == 0 ~ "fizz buzz"
#' )
#'
#' # Note that NA values in the vector x do not get special treatment. If you want
#' # to explicitly handle NA values you can use the `is.na` function:
#' x[2:4] <- NA_real_
#' case_when(
#'   x %% 35 == 0 ~ "fizz buzz",
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   is.na(x) ~ "nope",
#'   TRUE ~ as.character(x)
#' )
#'
#' # All RHS values need to be of the same type. Inconsistent types will throw an error.
#' # This applies also to NA values used in RHS: NA is logical, use
#' # typed values like NA_real_, NA_complex, NA_character_, NA_integer_ as appropriate.
#' case_when(
#'   x %% 35 == 0 ~ NA_character_,
#'   x %% 5 == 0 ~ "fizz",
#'   x %% 7 == 0 ~ "buzz",
#'   TRUE ~ as.character(x)
#' )
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA_real_
#' )
#'
#' # case_when() evaluates all RHS expressions, and then constructs its
#' # result by extracting the selected (via the LHS expressions) parts.
#' # In particular NaN are produced in this case:
#' y <- seq(-2, 2, by = .5)
#' case_when(
#'   y >= 0 ~ sqrt(y),
#'   TRUE   ~ y
#' )
#'
#' # This throws an error as NA is logical not numeric
#' \dontrun{
#' case_when(
#'   x %% 35 == 0 ~ 35,
#'   x %% 5 == 0 ~ 5,
#'   x %% 7 == 0 ~ 7,
#'   TRUE ~ NA
#' )
#' }
#'
#' # case_when is particularly useful inside mutate when you want to
#' # create a new variable that relies on a complex combination of existing
#' # variables
#' starwars %>%
#'   select(name:mass, gender, species) %>%
#'   mutate(
#'     type = case_when(
#'       height > 200 | mass > 200 ~ "large",
#'       species == "Droid"        ~ "robot",
#'       TRUE                      ~ "other"
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
#'     species == "Droid"        ~ "robot",
#'     TRUE                      ~ "other"
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
#'     height > 200 | mass > 200      ~ "large",
#'     if (robots) species == "Droid" ~ "robot",
#'     TRUE                           ~ "other"
#'   )
#' }
#'
#' starwars %>%
#'   mutate(type = case_character_type(height, mass, species, robots = FALSE)) %>%
#'   pull(type)
case_when <- function(...) {
  fs <- compact_null(list2(...))
  n <- length(fs)

  if (n == 0) {
    abort("No cases provided")
  }

  query <- vector("list", n)
  value <- vector("list", n)

  default_env <- caller_env()
  quos_pairs <- map2(fs, seq_along(fs), validate_formula, default_env, current_env())

  for (i in seq_len(n)) {
    pair <- quos_pairs[[i]]
    query[[i]] <- eval_tidy(pair$lhs, env = default_env)
    value[[i]] <- eval_tidy(pair$rhs, env = default_env)

    if (!is.logical(query[[i]])) {
      abort_case_when_logical(pair$lhs, i, query[[i]])
    }
  }

  m <- validate_case_when_length(query, value, fs)

  out <- value[[1]][rep(NA_integer_, m)]
  replaced <- rep(FALSE, m)

  for (i in seq_len(n)) {
    out <- replace_with(out, query[[i]] & !replaced, value[[i]], NULL)
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }

  out
}

validate_formula <- function(x, i, default_env, dots_env) {
  # Formula might be quosured
  if (is_quosure(x)) {
    default_env <- quo_get_env(x)
    x <- quo_get_expr(x)
  }

  if (!is_formula(x)) {
    arg <- substitute(...(), dots_env)[[1]]
    abort_case_when_formula(arg, i, x)
  }
  if (is_null(f_lhs(x))) {
    abort("formulas must be two-sided")
  }

  # Formula might be unevaluated, e.g. if it's been quosured
  env <- f_env(x) %||% default_env

  list(
    lhs = new_quosure(f_lhs(x), env),
    rhs = new_quosure(f_rhs(x), env)
  )
}

abort_case_when_formula <- function(arg, i, obj) {
  deparsed <- fmt_obj1(deparse_trunc(arg))
  type <- friendly_type_of(obj)
  abort(glue("Case {i} ({deparsed}) must be a two-sided formula, not {type}"))
}

abort_case_when_logical <- function(lhs, i, query) {
  deparsed <- fmt_obj1(deparse_trunc(quo_squash(lhs)))
  type <- friendly_type_of(query)
  abort(glue("LHS of case {i} ({deparsed}) must be a logical vector, not {type}"))
}

validate_case_when_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))

  if (length(all_lengths) <= 1) {
    return(all_lengths[[1]])
  }

  non_atomic_lengths <- all_lengths[all_lengths != 1]
  len <- non_atomic_lengths[[1]]

  if (length(non_atomic_lengths) == 1) {
    return(len)
  }

  inconsistent_lengths <- non_atomic_lengths[-1]
  lhs_problems <- lhs_lengths %in% inconsistent_lengths
  rhs_problems <- rhs_lengths %in% inconsistent_lengths
  problems <- lhs_problems | rhs_problems

  bad_calls(
    fs[problems],
    check_length_val(inconsistent_lengths, len, header = NULL, .abort = identity)
  )
}
