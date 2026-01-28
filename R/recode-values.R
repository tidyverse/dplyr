#' Recode and replace values
#'
#' @description
#' `recode_values()` and `replace_values()` provide two ways to map old values
#' to new values. They work by matching values against `x` and using the first
#' match to determine the corresponding value in the output vector. You can also
#' think of these functions as a way to use a lookup table to recode a vector.
#'
#' - Use `recode_values()` when creating an entirely new vector.
#'
#' - Use `replace_values()` when partially updating an existing vector.
#'
#' If you are just replacing a few values within an existing vector, then
#' `replace_values()` is always a better choice because it is type stable and
#' better expresses intent.
#'
#' A major difference between the two functions is what happens when no cases
#' match:
#'
#' - `recode_values()` falls through to a `default`.
#'
#' - `replace_values()` retains the original values from `x`.
#'
#' These functions have two mutually exclusive ways to use them:
#'
#' - A formula-based approach, i.e. `recode_values(x, from1 ~ to1, from2 ~
#'   to2)`, similar to [case_when()], which is useful when you have a small
#'   number of cases.
#'
#' - A vector-based approach, i.e. `recode_values(x, from = from, to = to)`,
#'   which is useful when you have a pre-built lookup table (which may come
#'   from an external source, like a CSV file).
#'
#' See `vignette("recoding-replacing")` for more examples.
#'
#' @param x A vector.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas. The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   - The LHS inputs can be any size, but will be
#'     [cast][vctrs::theory-faq-coercion] to the type of `x`.
#'
#'   - The RHS inputs will be [recycled][vctrs::theory-faq-recycling] to the
#'     same size as `x`. For `recode_values()` they will be
#'     [cast][vctrs::theory-faq-coercion] to their common type, and for
#'     `replace_values()` they will be [cast][vctrs::theory-faq-coercion] to the
#'     type of `x`.
#'
#'   `NULL` inputs are ignored.
#'
#'   Mutually exclusive with `from` and `to`.
#'
#' @param from Values to look up in `x` and map to values in `to`.
#'
#'   Typically this is a single vector of any size that is
#'   [cast][vctrs::theory-faq-coercion] to the type of `x`. For more advanced
#'   usage, this can be a list of vectors of any size each of which are
#'   [cast][vctrs::theory-faq-coercion] to the type of `x`.
#'
#'   Mutually exclusive with `...`.
#'
#' @param to Values that `from` map to.
#'
#'   Typically this is a single vector that is
#'   [recycled][vctrs::theory-faq-recycling] to the size of `from`. For more
#'   advanced usage, this can be a list of vectors each of which are
#'   [recycled][vctrs::theory-faq-recycling] to the size of `x`.
#'
#'   Mutually exclusive with `...`.
#'
#' @param default Default value to use when there is a value present in `x`
#'   that is unmatched by a value in `from`.
#'
#'   By default, a missing value is used as the default value.
#'
#'   If supplied, will be [recycled][vctrs::theory-faq-recycling] to the size of
#'   `x`.
#'
#'   Can only be set when `unmatched = "default"`.
#'
#' @param unmatched Handling of unmatched locations.
#'
#'   One of:
#'
#'   - `"default"` to use `default` in unmatched locations.
#'
#'   - `"error"` to error when there are unmatched locations.
#'
#' @param ptype An optional override for the output type, which is usually
#'   computed as the common type of `to` and `default`.
#'
#' @returns
#' A vector the same size as `x`.
#'
#' - For `recode_values()`, the type of the output is computed as the common
#'   type of `to` and `default`, unless overridden by `ptype`. The names of the
#'   output come from the names of `to` and `default`.
#'
#' - For `replace_values()`, the type of the output will have the same type
#'   as `x`. The names of the output will be the same as the names of `x`.
#'
#' @seealso [case_when()], [vctrs::vec_recode_values()]
#'
#' @name recode-and-replace-values
#'
#' @examples
#' x <- c("NC", "NYC", "CA", NA, "NYC", "Unknown")
#'
#' # `recode_values()` is useful for fully recoding from one set of values to
#' # another, creating an entirely new vector in the process. Note that any
#' # unmatched values result in `NA`, or a `default` value.
#' recode_values(
#'   x,
#'   "NC" ~ "North Carolina",
#'   "NYC" ~ "New York",
#'   "CA" ~ "California"
#' )
#'
#' recode_values(
#'   x,
#'   "NC" ~ "North Carolina",
#'   "NYC" ~ "New York",
#'   "CA" ~ "California",
#'   default = "<not recorded>"
#' )
#'
#' # `replace_values()` is useful for updating an existing vector, tweaking a
#' # few values along the way
#' replace_values(x, "NYC" ~ "NY")
#'
#' # `replace_values()` is particularly nice for replacing `NA`s with values...
#' replace_values(x, NA ~ "Unknown (NA)")
#' # ...or values with `NA`s
#' replace_values(x, "Unknown" ~ NA)
#'
#' # Multiple values can be grouped within a single left-hand side to normalize
#' # all problematic values at once
#' replace_values(x, c(NA, "Unknown") ~ "<not recorded>")
#'
#' # ---------------------------------------------------------------------------
#' # Lookup tables
#'
#' # `recode_values()` works with more than just character vectors. Imagine you
#' # have this series of Likert Scale scores, which is a scoring system that is
#' # ordered from 1-5.
#' data <- tibble(
#'   score = c(1, 2, 3, 4, 5, 2, 3, 1, 4)
#' )
#'
#' # To recode each `score` to its corresponding Likert Score label, you may
#' # initially be inclined to reach for `case_when()`
#' data |>
#'   mutate(
#'     score = case_when(
#'       score == 1 ~ "Strongly disagree",
#'       score == 2 ~ "Disagree",
#'       score == 3 ~ "Neutral",
#'       score == 4 ~ "Agree",
#'       score == 5 ~ "Strongly agree"
#'     )
#'   )
#'
#' # While this works, it can be written more efficiently using
#' # `recode_values()`
#' data |>
#'   mutate(
#'     score = score |>
#'       recode_values(
#'         1 ~ "Strongly disagree",
#'         2 ~ "Disagree",
#'         3 ~ "Neutral",
#'         4 ~ "Agree",
#'         5 ~ "Strongly agree"
#'       )
#'   )
#'
#' # `recode_values()` actually has two mutually exclusive APIs. The formula API
#' # used above, which is like `case_when()`, and a lookup style API that uses
#' # `from` and `to` arguments. The lookup API is even better suited for this
#' # problem, because we can move the mapping outside of the `mutate()` call
#' # into a standalone lookup table. You could even imagine reading this
#' # `likert` lookup table in from a separate CSV file.
#' likert <- tribble(
#'   ~from, ~to,
#'   1, "Strongly disagree",
#'   2, "Disagree",
#'   3, "Neutral",
#'   4, "Agree",
#'   5, "Strongly agree"
#' )
#'
#' data |>
#'   mutate(score = recode_values(score, from = likert$from, to = likert$to))
#'
#' # You can utilize the same lookup table across multiple columns by using
#' # `across()`
#' data_months <- tibble(
#'   score_january = c(1, 2, 3, 4, 5, 2, 3, 1, 4),
#'   score_february = c(4, 2, 1, 2, 1, 5, 2, 4, 4)
#' )
#'
#' data_months |>
#'   mutate(across(
#'     starts_with("score"),
#'     ~ recode_values(.x, from = likert$from, to = likert$to)
#'   ))
#'
#' # The `unmatched` argument allows you to assert that you believe that you've
#' # recoded all of the cases and will error if you've missed one, adding an
#' # extra layer of safety
#' data_with_zero <- add_row(data, score = 0)
#'
#' try({
#'   recode_values(
#'     data_with_zero$score,
#'     from = likert$from,
#'     to = likert$to,
#'     unmatched = "error"
#'   )
#' })
#'
#' # Note that missing values are considered unmatched. If you expect missing
#' # values, you'll need to handle them explicitly in your lookup table.
#' data_with_missing <- add_row(data, score = NA)
#'
#' try({
#'   recode_values(
#'     data_with_missing$score,
#'     from = likert$from,
#'     to = likert$to,
#'     unmatched = "error"
#'   )
#' })
#'
#' likert <- add_row(likert, from = NA, to = NA)
#'
#' recode_values(
#'   data_with_missing$score,
#'   from = likert$from,
#'   to = likert$to,
#'   unmatched = "error"
#' )
#'
#' # ------------------------------------------------------------------------------
#' # Lists of vectors
#'
#' # In some cases, your mapping may collapse multiple groups together into a
#' # single value. For example, here we'd like to standardize the school names.
#' schools <- c(
#'   "UNC",
#'   "Chapel Hill",
#'   NA,
#'   "Duke",
#'   "Duke University",
#'   "UNC",
#'   "NC State",
#'   "ECU",
#'   "East Carolina"
#' )
#'
#' # This `tribble()` is more complex than it may appear, it actually
#' # creates a list column!
#' standardized <- tribble(
#'   ~from,                        ~to,
#'   c("UNC", "Chapel Hill"),      "UNC",
#'   c("Duke", "Duke University"), "Duke",
#'   c("NC State"),                "NC State",
#'   c("ECU", "East Carolina"),    "ECU",
#'   NA,                           NA
#' )
#'
#' standardized
#' standardized$from
#'
#' # `recode_values()` treats a list `from` value as a list of vectors, where
#' # any match within one of the vectors is mapped to its corresponding `to`
#' # value
#' recode_values(
#'   schools,
#'   from = standardized$from,
#'   to = standardized$to,
#'   unmatched = "error"
#' )
#'
#' # This formula based approach is equivalent, but the lookup based approach is
#' # nicer because the lookup table can be defined separately
#' recode_values(
#'   schools,
#'   c("UNC", "Chapel Hill") ~ "UNC",
#'   c("Duke", "Duke University") ~ "Duke",
#'   c("NC State") ~ "NC State",
#'   c("ECU", "East Carolina") ~ "ECU",
#'   NA ~ NA,
#'   unmatched = "error"
#' )
NULL

#' @rdname recode-and-replace-values
#' @export
recode_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  unmatched = "default",
  ptype = NULL
) {
  check_dots_unnamed()

  args <- eval_formulas_or_from_and_to(
    ...,
    from = from,
    to = to,
    allow_empty_dots = FALSE
  )
  from <- args$from
  to <- args$to
  from_as_list_of_vectors <- args$from_as_list_of_vectors
  to_as_list_of_vectors <- args$to_as_list_of_vectors
  from_arg <- args$from_arg
  to_arg <- args$to_arg

  vec_recode_values(
    x = x,
    from = from,
    to = to,
    default = default,
    unmatched = unmatched,
    from_as_list_of_vectors = from_as_list_of_vectors,
    to_as_list_of_vectors = to_as_list_of_vectors,
    ptype = ptype,
    x_arg = "x",
    from_arg = from_arg,
    to_arg = to_arg,
    default_arg = "default",
    error_call = current_env()
  )
}

#' @rdname recode-and-replace-values
#' @export
replace_values <- function(
  x,
  ...,
  from = NULL,
  to = NULL
) {
  check_dots_unnamed()

  args <- eval_formulas_or_from_and_to(
    ...,
    from = from,
    to = to,
    allow_empty_dots = TRUE
  )
  from <- args$from
  to <- args$to
  from_as_list_of_vectors <- args$from_as_list_of_vectors
  to_as_list_of_vectors <- args$to_as_list_of_vectors
  from_arg <- args$from_arg
  to_arg <- args$to_arg

  vec_replace_values(
    x = x,
    from = from,
    to = to,
    from_as_list_of_vectors = from_as_list_of_vectors,
    to_as_list_of_vectors = to_as_list_of_vectors,
    x_arg = "x",
    from_arg = from_arg,
    to_arg = to_arg,
    error_call = current_env()
  )
}

eval_formulas_or_from_and_to <- function(
  ...,
  from,
  to,
  allow_empty_dots,
  user_env = caller_env(2),
  error_call = caller_env()
) {
  implementation <- determine_implementation(
    ...,
    from = from,
    to = to,
    error_call = error_call
  )

  switch(
    implementation,
    "dots" = {
      from_as_list_of_vectors <- TRUE
      to_as_list_of_vectors <- TRUE
      from_arg <- ""
      to_arg <- ""

      args <- eval_formulas(
        ...,
        allow_empty_dots = allow_empty_dots,
        user_env = user_env,
        error_call = error_call
      )
      from <- args$lhs
      to <- args$rhs
    },
    "from-to" = {
      from_as_list_of_vectors <- obj_is_list(from)
      to_as_list_of_vectors <- obj_is_list(to)
      from_arg <- "from"
      to_arg <- "to"
    },
    abort("Unreachable", .internal = TRUE)
  )

  list(
    from = from,
    to = to,
    from_as_list_of_vectors = from_as_list_of_vectors,
    to_as_list_of_vectors = to_as_list_of_vectors,
    from_arg = from_arg,
    to_arg = to_arg
  )
}

determine_implementation <- function(..., from, to, error_call) {
  has_dots <- !missing(...)
  has_from <- !is.null(from)
  has_to <- !is.null(to)

  # Supplied `...`
  if (has_dots) {
    if (has_from) {
      cli::cli_abort(
        "Can't supply both {.arg from} and {.arg ...}.",
        call = error_call
      )
    }

    if (has_to) {
      cli::cli_abort(
        "Can't supply both {.arg to} and {.arg ...}.",
        call = error_call
      )
    }

    return("dots")
  }

  # Supplied `from` and `to`
  if (has_from || has_to) {
    if (!has_from || !has_to) {
      cli::cli_abort(
        "Must supply both {.arg from} and {.arg to}.",
        call = error_call
      )
    }

    return("from-to")
  }

  # Supplied nothing. We use `"dots"` here which lets `recode_values()` error
  # and `replace_values()` be a no-op.
  "dots"
}
