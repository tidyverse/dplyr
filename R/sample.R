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
  check_size(size, n, replace)

  idx <- sample.int(n, size, replace = replace, prob = weight)
  tbl[idx, , drop = FALSE]
}

# Grouped data frames ----------------------------------------------------------

sample_n.grouped_df <- function(tbl, size, replace = FALSE, weight = NULL,
                                .env = parent.frame()) {

  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  weight <- substitute(weight)

  index <- attr(tbl, "indices")
  sampled <- lapply(index, sample_group, frac = FALSE,
    tbl = tbl, size = size, replace = replace, weight = weight, .env = .env)
  idx <- unlist(sampled) + 1

  grouped_df(tbl[idx, , drop = FALSE], vars = groups(tbl))
}

sample_frac.grouped_df <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                                   .env = parent.frame()) {

  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  if (size > 1 && !replace) {
    stop("Sampled fraction can't be greater than one unless replace = TRUE",
      call. = FALSE)
  }
  weight <- substitute(weight)

  index <- attr(tbl, "indices")
  sampled <- lapply(index, sample_group, frac = TRUE,
    tbl = tbl, size = size, replace = replace, weight = weight, .env = .env)
  idx <- unlist(sampled) + 1

  grouped_df(tbl[idx, , drop = FALSE], vars = groups(tbl))
}

sample_group <- function(tbl, i, frac = FALSE, size, replace = TRUE,
                         weight = NULL, .env = parent.frame()) {
  n <- length(i)
  if (frac) size <- round(size * n)

  check_size(size, n, replace)

  # weight use standard evaluation in this function
  if (!is.null(weight)) {
    weight <- eval(weight, tbl[i + 1, , drop = FALSE], .env)
    weight <- check_weight(weight, n)
  }

  i[sample.int(n, size, replace = replace, prob = weight)]
}


# Data tables ------------------------------------------------------------------
# data.frame handles data.tables, but loses class of tbl_df

#' @export
sample_n.tbl_dt <- function(tbl, size, replace = FALSE, weight = NULL,
                            .env = parent.frame()) {
  tbl_dt(NextMethod())
}

#' @export
sample_frac.tbl_dt <- function(tbl, size = 1, replace = FALSE, weight = NULL,
                               .env = parent.frame()) {
  tbl_dt(NextMethod())
}

# Grouped data tables ----------------------------------------------------------

#' @export
sample_n.grouped_dt <- function(tbl, size, replace = FALSE, weight = NULL,
                                .env = parent.frame()) {

  idx_call <- substitute(
    .I[sample(.N, size = size, replace = replace, prob = weight)],
    list(size = size, replace = replace, weight = substitute(weight))
  )
  idx <- dt_col_compute(tbl, idx_call, .env)

  grouped_dt(tbl[idx], groups(tbl))
}

#' @export
sample_frac.grouped_dt <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  idx_call <- substitute(
    .I[sample(.N, size = round(size * .N), replace = replace, prob = weight)],
    list(size = size, replace = replace, weight = substitute(weight))
  )
  idx <- dt_col_compute(tbl, idx_call, .env)

  grouped_dt(tbl[idx], groups(tbl))
}

# Default method ---------------------------------------------------------------

#' @export
sample_n.default <- function(tbl, size, replace = FALSE, weight = NULL,
                             .env = parent.frame()) {

  stop("Don't know how to sample from objects of class ", class(tbl)[1],
    call. = FALSE)
}

#' @export
sample_frac.default <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  stop("Don't know how to sample from objects of class ", class(tbl)[1],
    call. = FALSE)
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

check_size <- function(size, n, replace = FALSE) {
  if (size <= n || replace) return()

  stop("Sample size (", size, ") greater than population size (", n, ").",
    " Do you want replace = TRUE?", call. = FALSE)
}
