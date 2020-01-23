#' Condense values into list-columns
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' `condense()` is an experimental variant of [summarise()] designed to make it
#' as easy as possible to work with list-columns. It is particularly powerful when
#' working with models: you can fit models per group with `condense()` and then
#' flexibly extract components with `summarise()`.
#'
#' It has three special properties:
#'
#' * It only ever returns a single row per group.
#' * Each new column is always wrapped in a list.
#' * It returns a [rowwise()] output.
#'
#' It's hard to explain `condense()` in isolation, so if you find these ideas
#' intriguing we recommend starting with `vignette("rowwise")`.
#'
#' @inheritParams arrange
#' @param ... <[`data-masking`][dplyr_data_masking]> Name-value pairs of
#'   functions. All outputs will be automatically wrapped in lists, making it
#'   most suitable for functions that that return non-vectors (e.g. linear
#'   models) or vectors of length greater than one.
#' @return
#' A [rowwise] object.
#'
#' * The rows and initial columns come from `group_data(.data)`.
#' * Each additional column that you provide will be wrapped in a list column.
#' * Output is grouped by row.
#' * Data frame attributes are **not** preserved, because `condense()`
#'   fundamentally creates a new data frame.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("condense")}.
#' @export
#' @examples
#' # Modelling ------------------------------------------------
#' mods <- mtcars %>%
#'   group_by(cyl) %>%
#'   condense(mod = lm(mpg ~ disp, data = across()))
#' mods
#'
#' mods %>%
#'   mutate(rsq = summary(mod)$r.squared)
#'
#' if (requireNamespace("broom")) {
#'   mods %>% summarise(broom::glance(mod))
#'   mods %>% summarise(broom::tidy(mod))
#'   mods %>% summarise(broom::augment(mod))
#' }
condense <- function(.data, ...) {
  UseMethod("condense")
}

#' @export
condense.data.frame <- function(.data, ...) {
  cols <- condense_cols(.data, ...)

  res <- group_keys(.data)
  res[names(cols)] <- cols

  rowwise(res, group_vars(.data))
}

condense_cols <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(..., .named = TRUE)
  dots_names <- names(dots)

  chunks <- vector("list", length(dots))

  tryCatch({
    for (i in seq_along(dots)) {
      chunks[[i]] <- mask$eval_all(dots[[i]])
      mask$add(dots_names[i], chunks[[i]])
    }

  },
  simpleError = function(e) {
    stop_eval_tidy(e, index = i, dots = dots, fn = "condense")
  })

  set_names(chunks, dots_names)
}
