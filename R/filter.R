#' Keep or drop rows that match a condition
#'
#' @description
#' These functions are used to subset a data frame, applying the expressions in
#' `...` to determine which rows should be kept (for `filter()`) or dropped (
#' for `filter_out()`).
#'
#' Multiple conditions can be supplied separated by a comma. These will be
#' combined with the `&` operator. To combine comma separated conditions using
#' `|` instead, wrap them in [when_any()].
#'
#' Both `filter()` and `filter_out()` treat `NA` like `FALSE`. This subtle
#' behavior can impact how you write your conditions when missing values are
#' involved. See the section on `Missing values` for important details and
#' examples.
#'
#' @inheritParams arrange
#' @inheritParams args_by
#'
#' @param ... <[`data-masking`][rlang::args_data_masking]> Expressions that
#'   return a logical vector, defined in terms of the variables in `.data`. If
#'   multiple expressions are included, they are combined with the `&` operator.
#'   To combine expressions using `|` instead, wrap them in [when_any()]. Only
#'   rows for which all expressions evaluate to `TRUE` are kept (for `filter()`)
#'   or dropped (for `filter_out()`).
#'
#' @param .preserve Relevant when the `.data` input is grouped. If `.preserve =
#'   FALSE` (the default), the grouping structure is recalculated based on the
#'   resulting data, otherwise the grouping is kept as is.
#'
#' @returns
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Rows are a subset of the input, but appear in the same order.
#' * Columns are not modified.
#' * The number of groups may be reduced (if `.preserve` is not `TRUE`).
#' * Data frame attributes are preserved.
#'
#' @section Missing values:
#'
#' Both `filter()` and `filter_out()` treat `NA` like `FALSE`. This results in
#' the following behavior:
#'
#' - `filter()` _drops_ both `NA` and `FALSE`.
#'
#' - `filter_out()` _keeps_ both `NA` and `FALSE`.
#'
#' This means that `filter(data, <conditions>) + filter_out(data, <conditions>)`
#' captures every row within `data` exactly once.
#'
#' The `NA` handling of these functions has been designed to match your
#' _intent_. When your intent is to keep rows, use `filter()`. When your intent
#' is to drop rows, use `filter_out()`.
#'
#' For example, if your goal with this `cars` data is to "drop rows where the
#' `class` is suv", then you might write this in one of two ways:
#'
#' ```{r}
#' cars <- tibble(class = c("suv", NA, "coupe"))
#' cars
#' ```
#'
#' ```{r}
#' cars |> filter(class != "suv")
#' ```
#'
#' ```{r}
#' cars |> filter_out(class == "suv")
#' ```
#'
#' Note how `filter()` drops the `NA` rows even though our goal was only to drop
#' `"suv"` rows, but `filter_out()` matches our intuition.
#'
#' To generate the correct result with `filter()`, you'd need to use:
#'
#' ```{r}
#' cars |> filter(class != "suv" | is.na(class))
#' ```
#'
#' This quickly gets unwieldy when multiple conditions are involved.
#'
#' In general, if you find yourself:
#'
#' - Using "negative" operators like `!=` or `!`
#' - Adding in `NA` handling like `| is.na(col)` or `& !is.na(col)`
#'
#' then you should consider if swapping to the other filtering variant would
#' make your conditions simpler.
#'
#' ## Comparison to base subsetting
#'
#' Base subsetting with `[` doesn't treat `NA` like `TRUE` or `FALSE`. Instead,
#' it generates a fully missing row, which is different from how both `filter()`
#' and `filter_out()` work.
#'
#' ```{r}
#' cars <- tibble(class = c("suv", NA, "coupe"), mpg = c(10, 12, 14))
#' cars
#' ```
#'
#' ```{r}
#' cars[cars$class == "suv",]
#'
#' cars |> filter(class == "suv")
#' ```
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
#' * [when_any()], [when_all()]
#'
#' @section Grouped tibbles:
#'
#' Because filtering expressions are computed within groups, they may yield
#' different results on grouped tibbles. This will be the case as soon as an
#' aggregating, lagging, or ranking function is involved. Compare this ungrouped
#' filtering:
#'
#' ```
#' starwars |> filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars |> filter(mass > mean(mass, na.rm = TRUE), .by = gender)
#' ```
#'
#' In the ungrouped version, `filter()` compares the value of `mass` in each row
#' to the global average (taken over the whole data set), keeping only the rows
#' with `mass` greater than this global average. In contrast, the grouped
#' version calculates the average mass separately for each `gender` group, and
#' keeps rows with `mass` greater than the relevant within-gender average.
#'
#' @section Methods:
#'
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("filter")}.
#'
#' @family single table verbs
#' @name filter
#'
#' @examples
#' # Filtering for one criterion
#' filter(starwars, species == "Human")
#'
#' # Filtering for multiple criteria within a single logical expression
#' filter(starwars, hair_color == "none" & eye_color == "black")
#' filter(starwars, hair_color == "none" | eye_color == "black")
#'
#' # Multiple comma separated expressions are combined using `&`
#' starwars |> filter(hair_color == "none", eye_color == "black")
#'
#' # To combine comma separated expressions using `|` instead, use `when_any()`
#' starwars |> filter(when_any(hair_color == "none", eye_color == "black"))
#'
#' # Filtering out to drop rows
#' filter_out(starwars, hair_color == "none")
#'
#' # When filtering out, it can be useful to first interactively filter for the
#' # rows you want to drop, just to double check that you've written the
#' # conditions correctly. Then, just change `filter()` to `filter_out()`.
#' filter(starwars, mass > 1000, eye_color == "orange")
#' filter_out(starwars, mass > 1000, eye_color == "orange")
#'
#' # The filtering operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' #
#' # The following keeps rows where `mass` is greater than the
#' # global average:
#' starwars |> filter(mass > mean(mass, na.rm = TRUE))
#'
#' # Whereas this keeps rows with `mass` greater than the per `gender`
#' # average:
#' starwars |> filter(mass > mean(mass, na.rm = TRUE), .by = gender)
#'
#' # If you find yourself trying to use a `filter()` to drop rows, then
#' # you should consider if switching to `filter_out()` can simplify your
#' # conditions. For example, to drop blond individuals, you might try:
#' starwars |> filter(hair_color != "blond")
#'
#' # But this also drops rows with an `NA` hair color! To retain those:
#' starwars |> filter(hair_color != "blond" | is.na(hair_color))
#'
#' # But explicit `NA` handling like this can quickly get unwieldy, especially
#' # with multiple conditions. Since your intent was to specify rows to drop
#' # rather than rows to keep, use `filter_out()`. This also removes the need
#' # for any explicit `NA` handling.
#' starwars |> filter_out(hair_color == "blond")
#'
#' # To refer to column names that are stored as strings, use the `.data`
#' # pronoun:
#' vars <- c("mass", "height")
#' cond <- c(80, 150)
#' starwars |>
#'   filter(
#'     .data[[vars[[1]]]] > cond[[1]],
#'     .data[[vars[[2]]]] > cond[[2]]
#'   )
#' # Learn more in ?rlang::args_data_masking
NULL

#' @rdname filter
#' @export
filter <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_by_typo(...)
  check_not_both_by_and_preserve({{ .by }}, .preserve)
  UseMethod("filter")
}

#' @rdname filter
#' @export
filter_out <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  check_by_typo(...)
  check_not_both_by_and_preserve({{ .by }}, .preserve)
  UseMethod("filter_out")
}

#' @export
filter.data.frame <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  filter_impl(
    .data = .data,
    ...,
    .by = {{ .by }},
    .preserve = .preserve,
    .verb = "filter"
  )
}

#' @export
filter_out.data.frame <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  filter_impl(
    .data = .data,
    ...,
    .by = {{ .by }},
    .preserve = .preserve,
    .verb = "filter_out"
  )
}

filter_impl <- function(
  .data,
  ...,
  .by,
  .preserve,
  .invert,
  .verb,
  .error_call = caller_env(),
  .user_env = caller_env(2)
) {
  dots <- dplyr_quosures(...)
  check_filter(dots, error_call = .error_call)

  by <- compute_by(
    by = {{ .by }},
    data = .data,
    by_arg = ".by",
    data_arg = ".data",
    error_call = .error_call
  )

  loc <- filter_rows(
    data = .data,
    dots = dots,
    by = by,
    verb = .verb,
    error_call = .error_call,
    user_env = .user_env
  )

  dplyr_row_slice(.data, loc, preserve = .preserve)
}

filter_rows <- function(
  data,
  dots,
  by,
  verb,
  error_call = caller_env(),
  user_env = caller_env(2)
) {
  error_call <- dplyr_error_call(error_call)

  mask <- DataMask$new(data, by, verb, error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  # 1:1 mapping between `dots` and `dots_expanded`
  dots_expanded <- filter_expand(dots, mask = mask, error_call = error_call)

  invert <- verb == "filter_out"

  filter_eval(
    dots = dots,
    dots_expanded = dots_expanded,
    invert = invert,
    mask = mask,
    error_call = error_call,
    user_env = user_env
  )
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
  env_filter <- env()

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

  new_quosures(dots)
}

# We evaluate `dots_expanded` but report errors relative to `dots` so that
# we show "In argument: `if_any(c(x, y), is.na)`" rather than its expanded form.
# This works because `dots` and `dots_expanded` have a 1:1 mapping.
filter_eval <- function(
  dots,
  dots_expanded,
  invert,
  mask,
  error_call = caller_env(),
  user_env = caller_env(2)
) {
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
    mask$eval_all_filter(dots_expanded, invert, env_filter),
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
  index <- cnd$dplyr_error_data$index
  result <- cnd$dplyr_error_data$result

  bullets <- cli::format_inline(
    "`..{index}` must be a logical vector, not {obj_type_friendly(result)}."
  )

  if (is.data.frame(result)) {
    # Provide some extra advice for people who try and use `across()` inside
    # of `filter()`
    bullets <- c(
      bullets,
      i = cli::format_inline(
        "If you used {.fn across} to generate this data frame, please use {.fn if_any} or {.fn if_all} instead."
      )
    )
  }

  bullets
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
    what = I("Using one column matrices in `filter()` or `filter_out()`"),
    with = I("one dimensional logical vectors"),
    env = env,
    user_env = user_env,
    always = TRUE,
    id = "dplyr-filter-one-column-matrix"
  )
}

check_not_both_by_and_preserve <- function(
  .by,
  .preserve,
  error_call = caller_env()
) {
  if (!quo_is_null(enquo(.by)) && !is_false(.preserve)) {
    abort("Can't supply both `.by` and `.preserve`.", call = error_call)
  }
  invisible(NULL)
}
