#' Row-wise data_frame creation
#'
#' Create a row-wise \code{\link{data_frame}}.
#'
#' @param ... Arguments specifying the structure of a \code{data_frame}.
#'
#' @export
#' @examples
#' frame_data(
#'   ~colA, ~colB,
#'   "a",   1,
#'   "b",   2
#' )
frame_data <- function(...) {

  dots <- list(...)

  # Extract the names.
  frame_names <- character()
  i <- 1
  while (TRUE) {

    el <- dots[[i]]
    if (!is.call(el))
      break

    if (!identical(el[[1]], as.name("~")))
      break

    if (length(el) != 2)
      stop("expected a column name with a single argument; e.g. '~ name'")

    candidate <- el[[2]]
    if (!(is.symbol(candidate) || is.character(candidate)))
        stop("expected a symbol or string denoting a column name")

    frame_names <- c(frame_names, as.character(el[[2]]))

    i <- i + 1
  }

  if (!length(frame_names))
    stop("no column names detected in 'frame_data()' call")

  frame_rest <- dots[i:length(dots)]
  n_elements <- length(frame_rest)

  # Figure out the associated number of rows and number of columns,
  # and validate that the supplied formula produces a rectangular
  # structure.
  frame_ncol <- length(frame_names)
  if (n_elements %% frame_ncol != 0) {
    stop(sprintf(
      "invalid 'frame_data()' specification: had %s elements and %s columns",
      n_elements,
      frame_ncol
    ))
  }

  frame_nrow <- n_elements / frame_ncol

  # Extract the columns from 'frame_rest'
  frame_columns <- lapply(seq_len(frame_ncol), function(i) {
    indices <- seq.default(from = i, to = length(frame_rest), by = frame_nrow)
    unlist(frame_rest[indices])
  })

  # Create a tbl_df and return it
  names(frame_columns) <- frame_names
  as_data_frame(frame_columns)
}

#' @rdname frame_data
#' @export
tibble <- frame_data
