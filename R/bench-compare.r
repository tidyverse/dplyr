#' Compare and benchmark srcs
#' 
#' These functions support the comparison of results and timings across
#' multiple sources.
#' 
#' Comparisons are performed using \code{equal_data_frame} so the order of
#' rows and columns are ignored.
#' 
#' @param srcs A list of \code{\link{srcs}}.
#' @param setup A function with a single argument that is called with each
#'   src. It should either return a \code{\link{tbl}} or a list of \code{tbl}s.
#' @param op A function with a single argument, the output of \code{setup}
#' @param comp For checking, an data frame to test results against. If not 
#'   supplied, defaults to the results from the first \code{src}.
#' @param times For benchmarking, the number of times each operation is 
#'   repeated.
#' @return 
#'   \code{compare_srcs}: an invisible \code{TRUE} on success, otherwise
#'   throws an error.
#'   
#'   \code{bench_srcs}: an object of class 
#'   \code{\link[microbenchmark]{microbenchmark}}
#' @seealso \code{\link{src_local}} for working with local data
#' @examples
#' if (require("Lahman") && require("microbenchmark")) {
#' lahman_local <- lahman_srcs("df", "dt", "cpp")
#' 
#' # A simple example single tbl
#' teams <- function(src) src %.% tbl("Teams")
#' y2010 <- function(tbl) tbl %.% filter(yearID == 2010)
#' 
#' compare_srcs(lahman_local, teams, y2010)
#' bench_srcs(lahman_local, teams, y2010)
#' 
#' # A more complicated example using multiple tables
#' setup <- function(src) {
#'   list(
#'     src %.% tbl("Batting") %.% filter(stint == 1) %.% select(playerID:H),
#'     src %.% tbl("Teams") %.% select(yearID, lgID, teamID, G, R:H)
#'   )
#' }
#' op <- function(tbls) {
#'   left_join(tbls[[1]], tbls[[2]], by = c("yearID", "teamID", "lgID"))
#' }
#' 
#' compare_srcs(lahman_local, setup, op)
#' bench_srcs(lahman_local, setup, op)
#' 
#' }
#' @name bench_compare
NULL

#' @export
#' @rdname bench_compare
bench_srcs <- function(srcs, setup, op, times = 10) {
  if (!require("microbenchmark")) {
    stop("Please install the microbenchmark package", call. = FALSE)
  }
  
  tbls <- lapply(srcs, setup)
  bench_tbls(srcs, op, times = times)
}

bench_tbls <- function(tbls, op, times = 10) {
  # Generate call to microbenchmark function that evaluates op for each tbl
  calls <- lapply(seq_along(srcs), function(i) {
    substitute(op(tbls[[i]]), list(i = i))
  })
  names(calls) <- names(srcs)
  
  mb <- as.call(c(quote(microbenchmark), calls, list(times = times)))
  eval(mb)  
}

#' @export
#' @rdname bench_compare
compare_srcs <- function(srcs, setup, op, ref = NULL, compare = equal_data_frame) {
  if (length(srcs) < 2) {
    stop("Need at least two srcs to compare", call. = FALSE)
  }
  
  tbls <- lapply(srcs, setup)
  compare_tbls(tbls, op, ref = ref, compare = compare)
}

#' @export
#' @rdname bench_compare
compare_tbls <- function(tbls, op, ref = NULL, compare = equal_data_frame) {
  if (!require("testthat")) {
    stop("Please install the testthat package", call. = FALSE)
  }
  
  if (length(tbls) < 2) {
    stop("Need at least two srcs to compare", call. = FALSE)
  }

  results <- lapply(tbls, function(x) as.data.frame(op(x)))

  if (is.null(ref)) {
    ref <- results[[1]]
    ref_name <- names(results)[1]
    rest <- results[-1]
  } else {
    rest <- results
    ref_name <- "supplied comparison"
  }
  
  for(i in seq_along(rest)) {
    ok <- compare(ref, rest[[i]])
#     if (!ok) browser()
    msg <- paste0(names(rest)[[i]], " not equal to ", ref_name)
    expect_true(ok, info = msg) 
  }

  invisible(TRUE)
}
