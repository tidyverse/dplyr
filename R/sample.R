#' Sample n rows from a table.
#'
#' This is a wrapper around \code{\link{sample.int}} to make it easy to
#' select random rows from a table. It currently only works for local
#' tbls.
#'
#' @param tbl tbl of data.
#' @param size For \code{sample_n}, the number of rows to select.
#'   For \code{sample_frac}, the fraction of rows to select.
#'   If \code{tbl} is grouped, \code{size} applies to each group.
#' @param replace Sample with or without replacement?
#' @param weight Sampling weights. This expression is evaluated in the
#'   context of the data frame. It must return a vector of non-negative
#'   numbers the same length as the input. Weights are automatically
#'   standardised to sum to 1.
#' @param .env Environment in which to look for non-data names used in
#'   \code{weight}. Non-default settings for experts only.
#' @name sample
#' @examples
#' # Sample fixed number per group
#' sample_n(mtcars, 10)
#' sample_n(mtcars, 50, replace = TRUE)
#' sample_n(mtcars, 10, weight = mpg)
#'
#' # Sample fixed fraction per group
#' # Default is to sample all data = randomly resample rows
#' sample_frac(mtcars)
#'
#' sample_frac(mtcars, 0.1)
#' sample_frac(mtcars, 1.5, replace = TRUE)
#' sample_frac(mtcars, 0.1, weight = 1 / mpg)
NULL

#' @rdname sample
#' @export
sample_n <- function(tbl, size, replace = FALSE, weight = NULL,
                     .env = parent.frame()) {
  UseMethod("sample_n")
}

#' @rdname sample
#' @export
sample_frac <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                        .env = parent.frame()) {
  UseMethod("sample_frac")
}

# Data frames (and tbl_df) -----------------------------------------------------

#' @export
sample_n.data.frame <- function(tbl, size, replace = FALSE, weight = NULL,
                                .env = parent.frame()) {
  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, size, replace = replace, weight = weight)
}


#' @export
sample_frac.data.frame <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  if (!missing(weight)) {
    weight <- eval(substitute(weight), tbl, .env)
  }

  sample_n_basic(tbl, round(size * nrow(tbl)), replace = replace, weight = weight)
}


sample_n_basic <- function(tbl, size, replace = FALSE, weight = NULL) {
  n <- nrow(tbl)

  weight <- check_weight(weight, n)
  assert_that(is.numeric(size), length(size) == 1, size >= 0)

  if (size > n && !replace) {
    stop("Sample size (", size, ") greater than population size (", n, ").",
      " Do you want replace = TRUE?", call. = FALSE)
  }

  idx <- sample.int(n, size, replace = replace, prob = weight)
  tbl[idx, , drop = FALSE]
}


# Helper functions -------------------------------------------------------------

check_weight <- function(x, n) {
  if (is.null(x)) return()

  if (!is.numeric(x)) {
    stop("Weights must be numeric", call. = FALSE)
  }
  if (any(x < 0)) {
    stop("Weights must all be greater than 0", call. = FALSE)
  }
  if (length(x) != n) {
    stop("Weights must be same length as data (", n, ")", call. = FALSE)
  }

  x / sum(x)
}
