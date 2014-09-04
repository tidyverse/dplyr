##' Make a Data Frame
##'
##' A trimmed down version of \code{\link{data.frame}} that:
##'
##' 1. Evaluates its arguments lazily and in order,
##' 2. Foregoes the option of providing \code{row.names},
##' 3. Foregoes column name checking (except that we confirm names are not empty),
##' 4. Does not convert strings to factors.
##'
##' @param ... A set of named arguments.
##' @examples
##' a <- 1:5
##' data_frame(x = a, y = x ^ 2, z = x + y, a = a + 1, b = a)
##' @export
data_frame <- function(...) {

  ## Get the dots used
  dots <- named_dots(...)
  dots_nm <- names(dots)
  n <- length(dots)

  ## Early escape for dots with no arguments
  if (n == 0) return(data.frame())

  ## Construct the list output
  output <- vector("list", n)
  names(output) <- character(n)

  ## Get reference to names
  output_nm <- names(output)

  ## Check the names
  if (any(!nzchar(names(dots)))) {
    stop("All arguments to `data_frame` should be named")
  }

  ## Fill the output
  i <- 1
  while (i <= n) {

    ## Fill by reference
    set_vector_elt(output, i, eval(dots[[i]], envir = output))
    set_string_elt(output_nm, i, dots_nm[[i]])

    ## Update
    i <- i + 1
  }

  ## Validate column lengths
  lengths <- vapply(output, NROW, integer(1))
  if (length(unique(lengths)) > 1) {
    stop("arguments imply differing number of rows: ",
         paste(lengths, collapse = ", "))
  }

  ## Set attributes
  setattr(output, "row.names", c(NA_integer_, -lengths[[1]]))
  setattr(output, "class", c("tbl_df", "tbl", "data.frame"))

  output
}
