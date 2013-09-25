#' Join data table tbls.
#' 
#' See \code{\link{join}} for a description of the general purpose of the 
#' functions. The data frame implementations are currently not terribly 
#' efficient.
#' 
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If \code{NULL}, the
#'   default, \code{join} will do a natural join, using all variables with 
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right - to suppress the message, supply
#'   a character vector.
#' @param copy If \code{y} is not a data frame or \code{\link{tbl_df}} and
#'   \code{copy} is \code{TRUE}, \code{y} will be converted into a data frame
#' @examples
#' data("Batting", package = "Lahman")
#' data("Master", package = "Lahman")
#' 
#' batting_df <- tbl_df(Batting)
#' person_df <- tbl_df(Master)
#' 
#' uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])
#' 
#' # Inner join: match batting and person data
#' inner_join(batting_df, person_df)
#' inner_join(batting_df, uperson_df)
#' 
#' # Left join: match, but preserve batting data
#' left_join(batting_df, uperson_df)
#' 
#' # Anti join: find batters without person data
#' anti_join(batting_df, person_df)
#' # or people who didn't bat
#' anti_join(person_df, batting_df)
#' 
#' @name join.tbl_df
NULL

#' @method inner_join tbl_df
#' @export
#' @rdname join.tbl_df
inner_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  grouped_df(NextMethod(), groups(x))
}

#' @method inner_join data.frame
#' @export
#' @rdname join.tbl_df
inner_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)

  keys <- join_keys(x, y, by = by)
  x.cols <- setdiff(names(x), by)
  y.cols <- setdiff(names(y), by)
  
  ids <- join_ids(keys)
  out <- cbind(
    x[ids$x,       , drop = FALSE], 
    y[ids$y, y.cols, drop = FALSE]
  )
  attr(out, "row.names") <- .set_row_names(nrow(out))
  out
}

#' @method left_join tbl_df
#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  grouped_df(NextMethod(), groups(x))
}

#' @method left_join data.frame
#' @export
#' @rdname join.tbl_df
left_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  
  keys <- join_keys(x, y, by = by)
  x.cols <- setdiff(names(x), by)
  y.cols <- setdiff(names(y), by)
  
  ids <- join_ids(keys, all = TRUE)
  out <- cbind(
    x[ids$x,       , drop = FALSE], 
    y[ids$y, y.cols, drop = FALSE]
  )
  attr(out, "row.names") <- .set_row_names(nrow(out))
  out
}

#' @method semi_join tbl_df
#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  grouped_df(NextMethod(), groups(x))
}

#' @method semi_join data.frame
#' @export
#' @rdname join.tbl_df
semi_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  
  keys <- join_keys(x, y, by = by)
  x.cols <- setdiff(names(x), by)
  y.cols <- setdiff(names(y), by)
  
  x[keys$x %in% keys$y, , drop = FALSE]
}

#' @method anti_join tbl_df
#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  grouped_df(NextMethod(), groups(x))
}

#' @method anti_join data.frame
#' @export
#' @rdname join.tbl_df
anti_join.data.frame <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  
  keys <- join_keys(x, y, by = by)
  x.cols <- setdiff(names(x), by)
  y.cols <- setdiff(names(y), by)
  
  x[!(keys$x %in% keys$y), , drop = FALSE]
}

# Basic tools -----------------------------------------------------------------
# These are not very efficient, but they get the job done

join_ids <- function(keys, all = FALSE) {
  ys <- split_indices(keys$y, keys$n)
  length(ys) <- keys$n
  
  if (all) {
    # replace NULL with NA to preserve those x's without matching y's
    nulls <- vapply(ys, function(x) length(x) == 0, logical(1))
    ys[nulls] <- list(NA_real_)
  }
  
  ys <- ys[keys$x]
  xs <- rep(seq_along(keys$x), vapply(ys, length, numeric(1)))
  
  list(x = xs, y = unlist(ys))
}

join_keys <- function(x, y, by = NULL) {
  by <- by %||% common_by(x, y)
  
  joint <- rbind(x[by], y[by])
  keys <- id(joint, drop = TRUE)
  
  n_x <- nrow(x)
  n_y <- nrow(y)
  
  list(
    x = keys[seq_len(n_x)],
    y = keys[n_x + seq_len(n_y)],
    n = attr(keys, "n")
  )
}
