#' Sample n rows from a table
#'
#' This is a wrapper around [sample.int()] to make it easy to
#' select random rows from a table. It currently only works for local
#' tbls.
#'
#' @param tbl tbl of data.
#' @param size <[`tidy-select`][dplyr_tidy_select]> For `sample_n()`, the number of rows to select.
#'   For `sample_frac()`, the fraction of rows to select.
#'   If `tbl` is grouped, `size` applies to each group.
#' @param replace Sample with or without replacement?
#' @param weight <[`tidy-select`][dplyr_tidy_select]> Sampling weights. This must evaluate to a vector of
#'   non-negative numbers the same length as the input. Weights are
#'   automatically standardised to sum to 1.
#' @param .env This variable is deprecated and no longer has any
#'   effect. To evaluate `weight` in a particular context, you can
#'   now unquote a [quosure][rlang::quosure].
#' @param ... ignored
#'
#' @name sample
#' @examples
#' by_cyl <- mtcars %>% group_by(cyl)
#'
#' # Sample fixed number per group
#' sample_n(mtcars, 10)
#' sample_n(mtcars, 50, replace = TRUE)
#' sample_n(mtcars, 10, weight = mpg)
#'
#' sample_n(by_cyl, 3)
#' sample_n(by_cyl, 10, replace = TRUE)
#' sample_n(by_cyl, 3, weight = mpg / mean(mpg))
#'
#' # Sample fixed fraction per group
#' # Default is to sample all data = randomly resample rows
#' sample_frac(mtcars)
#'
#' sample_frac(mtcars, 0.1)
#' sample_frac(mtcars, 1.5, replace = TRUE)
#' sample_frac(mtcars, 0.1, weight = 1 / mpg)
#'
#' sample_frac(by_cyl, 0.2)
#' sample_frac(by_cyl, 1, replace = TRUE)
NULL

#' @rdname sample
#' @export
sample_n <- function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...) {
  UseMethod("sample_n")
}
#' @rdname sample
#' @export
sample_frac <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...) {
  UseMethod("sample_frac")
}

#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE,
                                weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), check_size(!!size, n(), replace = replace), replace = replace, prob = !!weight))
}
#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE,
                                   weight = NULL, .env = NULL, ...) {
  if (!is_null(.env)) {
    inform("`.env` is deprecated and no longer has any effect")
  }

  size <- enquo(size)
  weight <- enquo(weight)

  slice(tbl, sample.int(n(), round(n() * check_frac(!!size, replace = replace)), replace = replace, prob = !!weight))
}

#' @export
sample_n.default <- function(tbl, size, replace = FALSE, weight = NULL,
                             .env = parent.frame(), ...) {
  bad_args("tbl", "must be a data frame, not {friendly_type_of(tbl)}")
}
#' @export
sample_frac.default <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                                .env = parent.frame(), ...) {
  bad_args("tbl", "must be a data frame, not {friendly_type_of(tbl)}")
}

# Helper functions -------------------------------------------------------------

check_weight <- function(x, n) {
  if (is.null(x)) return()

  if (!is.numeric(x)) {
    bad_args("weight", "must be a numeric, not {friendly_type_of(x)}")
  }
  if (any(x < 0)) {
    bad_args("weight", "must be a vector with all values nonnegative, ",
      "not {x[x < 0][[1]]}"
    )
  }
  if (length(x) != n) {
    bad_args("weight", "must be a length {n} (same as data), ",
      "not {length(x)}"
    )
  }

  x / sum(x)
}

check_size <- function(size, n, replace = FALSE) {
  if (size <= n || replace) return(invisible(size))

  bad_args("size", "must be less or equal than {n} (size of data), ",
    "set `replace` = TRUE to use sampling with replacement"
  )
}

check_frac <- function(size, replace = FALSE) {
  if (size <= 1 || replace) return(invisible(size))

  bad_args("size", "of sampled fraction must be less or equal to one, ",
    "set `replace` = TRUE to use sampling with replacement"
  )
}
