#' Subset rows using column values
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
#' @param ... <[`data-masking`][dplyr_data_masking]> Expressions that return a
#'   logical value, and are defined in terms of the variables in `.data`.
#'   If multiple expressions are included, they are combined with the `&` operator.
#'   Only rows for which all conditions evaluate to `TRUE` are kept.
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
#' # Learn more in ?dplyr_data_masking
filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}

#' @export
filter.data.frame <- function(.data, ..., .preserve = FALSE) {
  loc <- filter_rows(.data, ..., caller_env = caller_env())
  dplyr_row_slice(.data, loc, preserve = .preserve)
}

filter_rows <- function(.data, ..., caller_env, error_call = caller_env()) {
  error_call <- dplyr_error_call(error_call)

  dots <- dplyr_quosures(...)
  check_filter(dots, error_call = error_call)

  mask <- DataMask$new(.data, caller_env, "filter", error_call = error_call)
  on.exit(mask$forget(), add = TRUE)

  dots <- filter_expand(dots, mask = mask, error_call = error_call)
  filter_eval(dots, mask = mask, error_call = error_call)
}

check_filter <- function(dots, error_call = error_call) {
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
    expand_if_across(dot)
  }

  dots <- withCallingHandlers(
    imap(unname(dots), filter_expand_one),
    error = function(cnd) {
      local_error_context(dots = dots, .index = env_filter$current_expression, mask = mask)
      abort(cnd_bullet_header("expanding"), call = error_call, parent = cnd)
    }
  )

  new_quosures(flatten(dots))
}

filter_eval <- function(dots, mask, error_call = caller_env()) {
  env_filter <- env()

  withCallingHandlers({
    mask$eval_all_filter(dots, env_filter)
  }, error = function(e) {
    local_error_context(dots = dots, .index = env_filter$current_expression, mask = mask)

    bullets <- c(
      cnd_bullet_header("computing"),
      filter_bullets(e)
    )
    abort(bullets, call = error_call, parent = skip_internal_condition(e))

  })
}

filter_bullets <- function(cnd, ...) {
  UseMethod("filter_bullets")
}
#' @export
filter_bullets.default <- function(cnd, ...) {
  c(i = cnd_bullet_cur_group_label())
}

#' @export
`filter_bullets.dplyr:::filter_incompatible_type` <- function(cnd, ...) {
  column_name <- cnd$dplyr_error_data$column_name
  index       <- cnd$dplyr_error_data$index
  result      <- cnd$dplyr_error_data$result

  input_name <- if (is.null(column_name)) {
    glue("..{index}")
  }  else {
    glue("..{index}${column_name}")
  }
  c(
    x = glue("Input `{input_name}` must be a logical vector, not a {vec_ptype_full(result)}."),
    i = cnd_bullet_cur_group_label()
  )
}

#' @export
`filter_bullets.dplyr:::filter_incompatible_size` <- function(cnd, ...) {
  index         <- cnd$dplyr_error_data$index
  expected_size <- cnd$dplyr_error_data$expected_size
  size          <- cnd$dplyr_error_data$size

  c(
    x = glue("Input `..{index}` must be of size {or_1(expected_size)}, not size {size}."),
    i = cnd_bullet_cur_group_label()
  )
}

