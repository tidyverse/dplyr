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
#' @param columns A \code{\link[lazyeval]{lazy_dots}}.
#' @seealso \code{\link{as_data_frame}} to turn an existing list into
#'   a data frame.
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
  data_frame_(lazyeval::lazy_dots(...))
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
    output[[i]] <-  lazyeval::lazy_eval(columns[[i]], output)
    names(output)[i] <- col_names[[i]]

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
  if (max != 1L && any(short)) {
    output[short] <- lapply(output[short], rep, max)
  }

  # Set attributes
  attr(output, "row.names") <- c(NA_integer_, max)
  attr(output, "class") <- c("tbl_df", "tbl", "data.frame")

  output
}

#' Coerce a list to a data frame.
#'
#' \code{as.data.frame} is effectively a thin wrapper around \code{data.frame},
#' and hence is rather slow (because it calls \code{data.frame} on each element
#' before \code{cbind}ing together). \code{as_data_frame} just verifies that
#' the list is structured correctly (i.e. named, and each element is same
#' length) then sets class and row name attributes.
#'
#' @param x A list. Each element of the list must have the same length.
#' @export
#' @examples
#' l <- list(x = 1:500, y = runif(500), z = 500:1)
#' df <- as_data_frame(l)
#'
#' # Coercing to a data frame does not copy columns
#' changes(as_data_frame(l), as_data_frame(l))
#'
#' # as_data_frame is considerably simpler/faster than as.data.frame
#' # making it more suitable for use when you have things that are
#' # lists
#' \dontrun{
#' l2 <- replicate(26, sample(letters), simplify = FALSE)
#' names(l2) <- letters
#' microbenchmark::microbenchmark(
#'   as_data_frame(l2),
#'   as.data.frame(l2)
#' )
#' }
as_data_frame <- function(x) {
  stopifnot(is.list(x))

  if (any(names2(x) == "")) {
    stop("All elements must be named", call. = FALSE)
  }

  n <- unique(vapply(x, NROW, integer(1)))
  if (length(n) != 1) {
    stop("Columns are not all same length", call. = FALSE)
  }

  class(x) <- c("tbl_df", "tbl", "data.frame")
  attr(x, "row.names") <- .set_row_names(n)

  x
}

#' Convert row names to an explicit variable.
#'
#' @param df Input data frame with rownames.
#' @param var Name of variable to use
#' @export
#' @examples
#' mtcars %>%
#'   tbl_df() %>%
#'   print() %>%
#'   add_rownames()
add_rownames <- function(df, var = "rowname") {
  stopifnot(is.data.frame(df))

  df[[var]] <- rownames(df)
  rownames(df) <- NULL

  df
}
