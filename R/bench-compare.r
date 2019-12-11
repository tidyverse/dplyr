#' Evaluate, compare, benchmark operations of a set of srcs.
#'
#' These functions support the comparison of results and timings across
#' multiple sources.
#'
#' @param tbls,tbls_x,tbls_y A list of [tbl()]s.
#' @param op A function with a single argument, called often with each
#'   element of `tbls`.
#' @param ref For checking, a data frame to test results against. If not
#'   supplied, defaults to the results from the first `src`.
#' @param compare A function used to compare the results. Defaults to
#'   `equal_data_frame` which ignores the order of rows and columns.
#' @param times For benchmarking, the number of times each operation is
#'   repeated.
#' @param \dots
#'    For `compare_tbls()`: additional parameters passed on the
#'      `compare()` function
#'
#'    For `bench_tbls()`: additional benchmarks to run.
#' @return
#'   `eval_tbls()`: a list of data frames.
#'
#'   `compare_tbls()`: an invisible `TRUE` on success, otherwise
#'   an error is thrown.
#'
#'   `bench_tbls()`: an object of class
#'   [microbenchmark::microbenchmark()]
#' @seealso [src_local()] for working with local data
#' @examples
#' \dontrun{
#' if (require("microbenchmark") && has_lahman()) {
#' lahman_local <- lahman_srcs("df", "sqlite")
#' teams <- lapply(lahman_local, function(x) x %>% tbl("Teams"))
#'
#' compare_tbls(teams, function(x) x %>% filter(yearID == 2010))
#' bench_tbls(teams, function(x) x %>% filter(yearID == 2010))
#'
#' # You can also supply arbitrary additional arguments to bench_tbls
#' # if there are other operations you'd like to compare.
#' bench_tbls(teams, function(x) x %>% filter(yearID == 2010),
#'    base = subset(Lahman::Teams, yearID == 2010))
#'
#' # A more complicated example using multiple tables
#' setup <- function(src) {
#'   list(
#'     src %>% tbl("Batting") %>% filter(stint == 1) %>% select(playerID:H),
#'     src %>% tbl("Master") %>% select(playerID, birthYear)
#'   )
#' }
#' two_tables <- lapply(lahman_local, setup)
#'
#' op <- function(tbls) {
#'   semi_join(tbls[[1]], tbls[[2]], by = "playerID")
#' }
#' # compare_tbls(two_tables, op)
#' bench_tbls(two_tables, op, times = 2)
#'
#' }
#' }
#' @name bench_compare
#' @keywords internal
NULL

#' @export
#' @rdname bench_compare
bench_tbls <- function(tbls, op, ..., times = 10) {
  check_pkg("microbenchmark", "compute table benchmarks")

  # Generate call to microbenchmark function that evaluates op for each tbl
  calls <- lapply(seq_along(tbls), function(i) {
    substitute(op(tbls[[i]]), list(i = i))
  })
  names(calls) <- names(tbls)

  mb <- as.call(c(
    quote(microbenchmark::microbenchmark), calls, dots(...),
    list(times = times)
  ))
  eval(mb)
}

#' @export
#' @rdname bench_compare
compare_tbls <- function(tbls, op, ref = NULL, compare = equal_data_frame, ...) {
  results <- eval_tbls(tbls, op)
  expect_equal_tbls(results, compare = compare, ...)
}

#' @export
#' @rdname bench_compare
compare_tbls2 <- function(tbls_x, tbls_y, op, ref = NULL, compare = equal_data_frame, ...) {
  results <- eval_tbls2(tbls_x, tbls_y, op)
  expect_equal_tbls(results, compare = compare, ...)
}

expect_equal_tbls <- function(results, ref = NULL, compare = equal_data_frame, ...) {
  check_pkg("testthat", "compare tables")

  if (length(results) < 2 && is.null(ref)) {
    testthat::skip("Need at least two srcs to compare")
  }


  if (is.null(ref)) {
    ref <- results[[1]]
    ref_name <- names(results)[1]
    rest <- results[-1]
  } else {
    rest <- results
    ref_name <- "supplied comparison"
  }

  for (i in seq_along(rest)) {
    ok <- compare(ref, rest[[i]], ...)
    # if (!ok) browser()
    msg <- paste0(
      names(rest)[[i]], " not equal to ", ref_name, "\n",
      attr(ok, "comment")
    )
    testthat::expect_true(ok, info = msg)
  }

  invisible(TRUE)
}

#' @export
#' @rdname bench_compare
eval_tbls <- function(tbls, op) {
  lapply(tbls, function(x) as.data.frame(op(x)))
}

#' @export
#' @rdname bench_compare
eval_tbls2 <- function(tbls_x, tbls_y, op) {
  Map(function(x, y) as.data.frame(op(x, y)), tbls_x, tbls_y)
}
