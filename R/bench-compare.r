#' Evaluate, compare, benchmark operations of a set of srcs.
#' 
#' These functions support the comparison of results and timings across
#' multiple sources.
#' 
#' @param tbls A list of \code{\link{tbl}}s.
#' @param op A function with a single argument, called often with each
#'   element of \code{tbls}.
#' @param ref For checking, an data frame to test results against. If not 
#'   supplied, defaults to the results from the first \code{src}.
#' @param compare A function used to compare the results. Defaults to 
#'   \code{equal_data_frame} which ignores the order of rows and columns.
#' @param times For benchmarking, the number of times each operation is 
#'   repeated.
#' @param \dots Additional parameters for the \code{compare} function
#' @return 
#'   \code{eval_tbls}: a list of data frames.
#' 
#'   \code{compare_tbl}: an invisible \code{TRUE} on success, otherwise
#'   an error is thrown.
#'   
#'   \code{bench_tbls}: an object of class 
#'   \code{\link[microbenchmark]{microbenchmark}}
#' @seealso \code{\link{src_local}} for working with local data
#' @examples
#' if (require("Lahman") && require("microbenchmark")) {
#' lahman_local <- lahman_srcs("df", "dt", "cpp")
#' teams <- lapply(lahman_local, function(x) x %.% tbl("Teams"))
#' 
#' compare_tbls(teams, function(x) x %.% filter(yearID == 2010))
#' bench_tbls(teams, function(x) x %.% filter(yearID == 2010))
#' 
#' # A more complicated example using multiple tables
#' setup <- function(src) {
#'   list(
#'     src %.% tbl("Batting") %.% filter(stint == 1) %.% select(playerID:H),
#'     src %.% tbl("Master") %.% select(playerID, birthYear)
#'   )
#' }
#' two_tables <- lapply(lahman_local, setup)
#' 
#' op <- function(tbls) {
#'   semi_join(tbls[[1]], tbls[[2]], by = "playerID")
#' }
#' compare_tbls(two_tables, op)
#' bench_tbls(two_tables, op, times = 2)
#' 
#' }
#' @name bench_compare
NULL

#' @export
#' @rdname bench_compare
bench_tbls <- function(tbls, op, times = 10) {
  if (!require("microbenchmark")) {
    stop("Please install the microbenchmark package", call. = FALSE)
  }

  # Generate call to microbenchmark function that evaluates op for each tbl
  calls <- lapply(seq_along(tbls), function(i) {
    substitute(op(tbls[[i]]), list(i = i))
  })
  names(calls) <- names(tbls)
  
  mb <- as.call(c(quote(microbenchmark), calls, list(times = times)))
  eval(mb)  
}

#' @export
#' @rdname bench_compare
compare_tbls <- function(tbls, op, ref = NULL, compare = equal_data_frame, ...) {
  if (length(tbls) < 2) {
    stop("Need at least two srcs to compare", call. = FALSE)
  }
  if (!require("testthat")) {
    stop("Please install the testthat package", call. = FALSE)
  }
  
  results <- eval_tbls(tbls, op)
  
  if (is.null(ref)) {
    ref <- results[[1]]
    ref_name <- names(results)[1]
    rest <- results[-1]
  } else {
    rest <- results
    ref_name <- "supplied comparison"
  }
  
  for(i in seq_along(rest)) {
    ok <- compare(ref, rest[[i]], ...)
    # if (!ok) browser()
    msg <- paste0(names(rest)[[i]], " not equal to ", ref_name)
    expect_true(ok, info = msg) 
  }
  
  invisible(TRUE)
}


#' @export
#' @rdname bench_compare
eval_tbls <- function(tbls, op) {
  lapply(tbls, function(x) as.data.frame(op(x)))
}

