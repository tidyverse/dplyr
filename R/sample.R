#' Sample n rows from a table
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' `sample_n()` and `sample_frac()` have been superseded in favour of
#' [slice_sample()]. While they will not be deprecated in the near future,
#' retirement means that we will only perform critical bug fixes, so we recommend
#' moving to the newer alternative.
#'
#' These functions were superseded because we realised it was more convenient to
#' have two mutually exclusive arguments to one function, rather than two
#' separate functions. This also made it to clean up a few other smaller
#' design issues with `sample_n()`/`sample_frac`:
#'
#' * The connection to `slice()` was not obvious.
#' * The name of the first argument, `tbl`, is inconsistent with other
#'   single table verbs which use `.data`.
#' * The `size` argument uses tidy evaluation, which is surprising and
#'   undocumented.
#' * It was easier to remove the deprecated `.env` argument.
#' * `...` was in a suboptimal position.
#'
#' @keywords internal
#' @param tbl A data.frame.
#' @param size <[`tidy-select`][dplyr_tidy_select]>
#'   For `sample_n()`, the number of rows to select.
#'   For `sample_frac()`, the fraction of rows to select.
#'   If `tbl` is grouped, `size` applies to each group.
#' @param replace Sample with or without replacement?
#' @param weight <[`tidy-select`][dplyr_tidy_select]> Sampling weights.
#'   This must evaluate to a vector of non-negative numbers the same length as
#'   the input. Weights are automatically standardised to sum to 1.
#' @param .env DEPRECATED.
#' @param ... ignored
#' @examples
#' by_cyl <- mtcars %>% group_by(cyl)
#'
#' # sample_n() -> slice_sample() ----------------------------------------------
#' sample_n(mtcars, 10)
#' sample_n(mtcars, 50, replace = TRUE)
#' sample_n(mtcars, 10, weight = mpg)
#'
#' # Changes:
#' # * explicitly name the `n` argument,
#' # * the `weight` argument is now `weight_by`.
#'
#' slice_sample(mtcars, n = 10)
#' slice_sample(mtcars, n = 50, replace = TRUE)
#' slice_sample(mtcars, n = 10, weight_by = mpg)
#'
#' # Note that sample_n() would error if n was bigger than the group size
#' # slice_sample() will just use the available rows for consistency with
#' # the other slice helpers like slice_head()
#'
#' # sample_frac() -> slice_sample() -------------------------------------------
#' sample_frac(mtcars)
#' sample_frac(mtcars, replace = TRUE)
#'
#' # Changes:
#' # * use prop = 1 to randomly sample all rows
#'
#' slice_sample(mtcars, prop = 1)
#' slice_sample(mtcars, prop = 1, replace = TRUE)
#'
#' @export
sample_n <- function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...) {
  lifecycle::signal_stage("superseded", "sample_n()")
  UseMethod("sample_n")
}

#' @export
sample_n.default <- function(tbl, size, replace = FALSE, weight = NULL,
                             .env = parent.frame(), ...) {
  msg <- glue("`tbl` must be a data frame, not {friendly_type_of(tbl)}.")
  abort(msg)
}

#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE,
                                weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`sample_n() argument `.env` is deprecated and no longer has any effect.")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  dplyr_local_error_call()
  slice(tbl, local({
    size <- check_size(!!size, n(), replace = replace)
    sample.int(n(), size, replace = replace, prob = !!weight)
  }))

}

#' @rdname sample_n
#' @export
sample_frac <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...) {
  lifecycle::signal_stage("superseded", "sample_frac()")
  UseMethod("sample_frac")
}

#' @export
sample_frac.default <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                                .env = parent.frame(), ...) {
  msg <- glue("`tbl` must be a data frame, not {friendly_type_of(tbl)}.")
  abort(msg)
}

#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE,
                                   weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  dplyr_local_error_call()
  slice(tbl, local({
    size <- round(n() * check_frac(!!size, replace = replace))
    sample.int(n(), size, replace = replace, prob = !!weight)
  }))
}


# Helper functions -------------------------------------------------------------

check_size <- function(size, n, replace = FALSE) {
  if (size <= n || replace) return(invisible(size))

  bullets <- c(
    glue("`size` must be less than or equal to {n} (size of data)."),
    i = "set `replace = TRUE` to use sampling with replacement."
  )
  abort(bullets, call = NULL)
}

check_frac <- function(size, replace = FALSE) {
  if (size <= 1 || replace) return(invisible(size))

  bullets <- c(
    glue("`size` of sampled fraction must be less or equal to one."),
    i = "set `replace = TRUE` to use sampling with replacement."
  )
  abort(bullets, call = NULL)
}
