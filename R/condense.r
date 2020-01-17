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
#' intriguing we recommending starting with `vignette("rowwise")`.
#'
#' @param .data a tbl
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs of functions.
#'   All outputs will be automatically wrapped in lists, making it most
#'   suitable for functions that that return non-vectors (e.g. linear models)
#'   or vectors of length greater than one.
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
  res
}

#' @export
condense.grouped_df <- function(.data, ...) {
  out <- NextMethod()
  rowwise(out, group_vars(.data))
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
