#' Build a data frame.
#'
#' A trimmed down version of \code{\link{data.frame}} that:
#' \enumerate{
#' \item Never coerces inputs (i.e. strings stay as strings!).
#' \item Never adds \code{row.names}.
#' \item Never munges column names.
#' \item Only recycles length 1 inputs.
#' \item Evaluates its arguments lazily and in order.
#' \item Adds \code{tbl_df} class to output.
#' }
#'
#' @param ... A set of named arguments
#' @param columns A \code{\link[lazy]{lazy_dots}}.
#' @export
#' @examples
#' a <- 1:5
#' data_frame(a, b = a * 2)
#' data_frame(a, b = a * 2, c = 1)
#' data_frame(x = runif(10), y = x * 2)
#'
#' # data_frame never coerces its inputs
#' str(data_frame(letters))
#' str(data_frame(x = diag(5)))
#'
#' # or munges column names
#' data_frame(`a + b` = 1:5)
data_frame <- function(...) {
  data_frame_(lazy::lazy_dots(...))
}

#' @export
#' @rdname data_frame
data_frame_ <- function(columns) {
  n <- length(columns)
  if (n == 0) return(data.frame())

  # If named not supplied, used deparsed expression
  col_names <- names2(columns)
  missing_names <- col_names == ""
  if (any(missing_names)) {
    deparse2 <- function(x) paste(deparse(x$expr, 500L), collapse = "")
    defaults <- vapply(columns[missing_names], deparse2, character(1),
      USE.NAMES = FALSE)

    col_names[missing_names] <- defaults
  }

  # Construct the list output
  output <- vector("list", n)
  names(output) <- character(n)
  output_nm <- names(output) # Get reference to names

  # Fill the output
  i <- 1L
  while (i <= n) {

    # Fill by reference
    set_vector_elt(output, i, lazy::lazy_eval(columns[[i]], output))
    set_string_elt(output_nm, i, col_names[[i]])

    # Update
    i <- i + 1L
  }

  # Validate column lengths
  lengths <- vapply(output, NROW, integer(1))
  max <- max(lengths)

  if (!all(lengths %in% c(1L, max))) {
    stop("arguments imply differing number of rows: ",
         paste(lengths, collapse = ", "))
  }
  short <- lengths == 1
  if (any(short)) {
    output[short] <- lapply(output[short], rep, max)
  }

  # Set attributes
  setattr(output, "row.names", c(NA_integer_, -lengths[[1]]))
  setattr(output, "class", c("tbl_df", "tbl", "data.frame"))

  output
}
