#' Do arbitrary computations
#'
#' @description
#' `do()` is a variant of [summarise()] designed to make it as easy as
#' possible to work with list-columns.  It is particularly powerful when
#' working with models: you can fit models per group with `do()` and then
#' flexibly extract components with `summarise()`.
#'
#' It has three special properties:
#'
#' * It only ever returns a single row per group.
#' * Each new column is always wrapped in a list.
#' * It returns a [rowwise()] output.
#'
#' It's hard to explain `do()` in isolation, so if you find these ideas
#' intriguing we recommending starting with `vignette("rowwise")`.
#'
#' @section dplyr 1.0.0:
#' We questioned the utility of `do()` for a long time, because something about
#' the way it worked just didn't click with the rest of dplyr. `do()` has
#' returned to active status in dplyr 1.0.0, although its scope is narrower
#' than in the past because two of its main features are now available in
#' more places:
#'
#' * `.` in `do()` used to be only way to easily compute on the entire
#'   data frame representing the "curent" group; now you can access that
#'   data frame in any verb with [across()].
#'
#' * You could supply a single unnamed argument that returned a data frame
#'   that would be unnested into the current group. You can now do this in
#'   `summarise()` since it gained support for multi-row and multi-column
#'   outputs.
#'
#' This allowed us to focus in the most useful aspect of `do()`: its ability
#' to easily generate list-columns and to return a [rowwise] data frame that
#' eliminates the need to manually vectorised functions with purrr's map
#' functions. Compared to previous versions, `do()` is also more useful because
#' it retains existing grouping.
#'
#' @param .data a tbl
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs of functions.
#'   Allow outputs will be automatically wrapped in lists, making it most
#'   suitable for functions that that return non-vectors (e.g. linear models)
#'   or vectors of length greater than one.
#' @export
#' @examples
#' # Modelling ------------------------------------------------
#' mods <- mtcars %>%
#'   group_by(cyl) %>%
#'   do(mod = lm(mpg ~ disp, data = across()))
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
do <- function(.data, ...) {
  if (dots_n(...) == 1 && !dots_named(...)) {
    # TODO: lifecycle::deprecate_warn() + translation
    abort("You need to use summarise() instead")
  }

  UseMethod("do")
}

#' @export
do.data.frame <- function(.data, ...) {
  cols <- sketch_cols(.data, ...)

  res <- group_keys(.data)
  res[names(cols)] <- cols
  res
}

#' @export
do.grouped_df <- function(.data, ...) {
  out <- NextMethod()
  rowwise(out, group_vars(.data))
}

sketch_cols <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  # TODO: add binding so that `.` and `.$var` works
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
    stop_eval_tidy(e, index = i, dots = dots, fn = "sketch")
  })

  set_names(chunks, dots_names)
}

# Helpers -----------------------------------------------------------------

dots_named <- function(...) {
  names <- names(substitute(alist(...)))
  any(names != "")
}
