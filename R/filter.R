#' Keep rows that match a condition
#'
#' The `filter()` function is used to subset a data frame,
#' retaining all rows that satisfy your conditions.
#' To be retained, the row must produce a value of `TRUE` for all conditions.
#' Note that when a condition evaluates to `NA`
#' the row will be dropped, unlike base subsetting with `[`.
#'
#' The `filter()` function is used to subset the rows of
#' `.data`, applying the expressions in `...` to the column values to determine which
#' rows should be retained. It can be applied to both grouped and ungrouped data (see [group_by()] and
#' [ungroup()]). However, dplyr is not yet smart enough to optimise the filtering
#' operation on grouped datasets that do not need grouped calculations. For this
#' reason, filtering is often considerably faster on ungrouped data.
#'
#' @section Useful filter functions:
#'
#' There are many functions and operators that are useful when constructing the
#' expressions used to filter the data:
#'
#' * [`==`], [`>`], [`>=`] etc
#' * [`&`], [`|`], [`!`], [xor()]
#' * [is.na()]
#' * [between()], [near()]
#'
#' @section Grouped tibbles:
#'
#' Because filtering expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped filtering:
#'
#' ```
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' In the ungrouped version, `filter()` compares the value of `mass` in each row to
#' the global average (taken over the whole data set), keeping only the rows with
#' `mass` greater than this global average. In contrast, the grouped version calculates
#' the average mass separately for each `gender` group, and keeps rows with `mass` greater
#' than the relevant within-gender average.
#'
#' @family single table verbs
#' @inheritParams arrange
#' @inheritParams args_by
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical value, and are defined in terms of the variables in
#'   `.data`. If multiple expressions are included, they are combined with the
#'   `&` operator. Only rows for which all conditions evaluate to `TRUE` are
#'   kept.
#' @param .preserve Relevant when the `.data` input is grouped.
#'   If `.preserve = FALSE` (the default), the grouping structure
#'   is recalculated based on the resulting data, otherwise the grouping is kept as is.
#' @return
#' An object of the same type as `.data`. The output has the following properties:
#'
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * The number of groups may be reduced (if `.preserve` is not `TRUE`).
#' * Data frame attributes are preserved.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("filter")}.
#' @export
#' @examples
#' # Filtering by one criterion
#' filter(starwars, species == "Human")
#' filter(starwars, mass > 1000)
#'
#' # Filtering by multiple criteria within a single logical expression
#' filter(starwars, hair_color == "none" & eye_color == "black")
#' filter(starwars, hair_color == "none" | eye_color == "black")
#'
#' # When multiple expressions are used, they are combined using &
#' filter(starwars, hair_color == "none", eye_color == "black")
#'
#'
#' # The filtering operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' #
#' # The following filters rows where `mass` is greater than the
#' # global average:
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#' # Whereas this keeps rows with `mass` greater than the gender
#' # average:
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#'
#' # To refer to column names that are stored as strings, use the `.data` pronoun:
#' vars <- c("mass", "height")
#' cond <- c(80, 150)
#' starwars %>%
#'   filter(
#'     .data[[vars[[1]]]] > cond[[1]],
#'     .data[[vars[[2]]]] > cond[[2]]
#'   )
#' # Learn more in ?rlang::args_data_masking
filter <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_by_typo(...)

  by <- enquo(.by)

  if (!quo_is_null(by) && !is_false(.preserve)) {
    abort("Can't supply both `.by` and `.preserve`.")
  }

  UseMethod("filter")
}

#' @export
filter.data.frame <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  dots <- dplyr_quosures(...)
  check_filter(dots)

  by <- compute_by(
    by = {{ .by }},
    data = .data,
    by_arg = ".by",
    data_arg = ".data"
  )

  loc <- filter_rows(.data, dots, by)
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

filter_rows <- function(data,
                        dots,
                        by,
                        error_call = caller_env(),
                        user_env = caller_env(2)) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, "filter", error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  dots <- filter_expand(dots, mask = mask, error_call = error_call)
  filter_eval(dots, mask = mask, error_call = error_call, user_env = user_env)
}

check_filter <- function(dots, error_call = caller_env()) {
  named <- have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # only allow named logical vectors, anything else
    # is suspicious
    expr <- quo_get_expr(quo)
    if (!is.logical(expr)) {
      name <- names(dots)[i]
      bullets <- c(
        "We detected a named input.",
        i = glue("This usually means that you've used `=` instead of `==`."),
        i = glue("Did you mean `{name} == {as_label(expr)}`?")
      )
      abort(bullets, call = error_call)
    }

  }
}

filter_expand <- function(dots, mask, error_call = caller_env()) {
  env_filter <-  env()
  filter_expand_one <- function(dot, index) {
    env_filter$current_expression <- index
    dot <- expand_pick(dot, mask)
    expand_if_across(dot)
  }

  local_error_context(dots, i = 0L, mask = mask)

  dots <- withCallingHandlers(
    imap(unname(dots), filter_expand_one),
    error = function(cnd) {
      poke_error_context(dots, env_filter$current_expression, mask = mask)
      abort(cnd_bullet_header("expand"), call = error_call, parent = cnd)
    }
  )

  dots <- list_flatten(dots)

  new_quosures(dots)
}

filter_eval <- function(dots,
                        mask,
                        error_call = caller_env(),
                        user_env = caller_env(2)) {
  env_filter <- env()
  warnings_state <- env(warnings = list())

  # For condition handlers
  i <- NULL
  env_bind_active(
    current_env(),
    "i" = function() env_filter$current_expression
  )

  warning_handler <- dplyr_warning_handler(
    state = warnings_state,
    mask = mask,
    error_call = error_call
  )

  out <- withCallingHandlers(
    mask$eval_all_filter(dots, env_filter),
    error = dplyr_error_handler(
      dots = dots,
      mask = mask,
      bullets = filter_bullets,
      error_call = error_call
    ),
    warning = function(cnd) {
      local_error_context(dots, i, mask)
      warning_handler(cnd)
    },
    `dplyr:::signal_filter_one_column_matrix` = function(e) {
      warn_filter_one_column_matrix(env = error_call, user_env = user_env)
    },
    `dplyr:::signal_filter_across` = function(e) {
      warn_filter_across(env = error_call, user_env = user_env)
    },
    `dplyr:::signal_filter_data_frame` = function(e) {
      warn_filter_data_frame(env = error_call, user_env = user_env)
    }
  )

  signal_warnings(warnings_state, error_call)
  out
}

filter_bullets <- function(cnd, ...) {
  UseMethod("filter_bullets")
}

#' @export
`filter_bullets.dplyr:::filter_incompatible_type` <- function(cnd, ...) {
  column_name <- cnd$dplyr_error_data$column_name
  index <- cnd$dplyr_error_data$index
  result <- cnd$dplyr_error_data$result

  if (is.null(column_name)) {
    input_name <- glue("..{index}")
  } else {
    input_name <- glue("..{index}${column_name}")
  }
  glue("`{input_name}` must be a logical vector, not {obj_type_friendly(result)}.")
}

#' @export
`filter_bullets.dplyr:::filter_incompatible_size` <- function(cnd, ...) {
  index <- cnd$dplyr_error_data$index
  expected_size <- cnd$dplyr_error_data$expected_size
  size <- cnd$dplyr_error_data$size

  glue("`..{index}` must be of size {or_1(expected_size)}, not size {size}.")
}

warn_filter_one_column_matrix <- function(env, user_env) {
  lifecycle::deprecate_warn(
    when = "1.1.0",
    what = I("Using one column matrices in `filter()`"),
    with = I("one dimensional logical vectors"),
    env = env,
    user_env = user_env
  )
}

warn_filter_across <- function(env, user_env) {
  lifecycle::deprecate_warn(
    when = "1.0.8",
    what = I("Using `across()` in `filter()`"),
    with = I("`if_any()` or `if_all()`"),
    always = TRUE,
    env = env,
    user_env = user_env
  )
}

warn_filter_data_frame <- function(env, user_env) {
  lifecycle::deprecate_warn(
    when = "1.0.8",
    what = I("Returning data frames from `filter()` expressions"),
    with = I("`if_any()` or `if_all()`"),
    always = TRUE,
    env = env,
    user_env = user_env
  )
}
