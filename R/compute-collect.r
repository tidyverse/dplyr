#' Compute a lazy tbl.
#' 
#' \code{compute} forces computation of lazy tbls, leaving data in the remote
#' source. \code{collect} also forces computation, but will bring data back into
#' an R data.frame (stored in a \code{\link{tbl_df}}).  
#' 
#' @param x a data tbl
#' @param ... other arguments passed on to methods
#' @seealso \code{\link{copy_to}} which is the conceptual opposite: it 
#'   takes a local data frame and makes it available to the remote source.
#' @export
#' @examples
#' batting <- tbl(lahman(), "Batting")
#' remote <- select(filter(tbl(l, "Batting"), year > 2010 && stint == 1), playerID:H)
#' cached <- compute(remote)
#' local  <- collect(remote)
compute <- function(x, name = random_table_name(), ...) {
  UseMethod("compute")
}

#' @export
#' @rdname compute
collect <- function(x, ...) {
  UseMethod("collect")
}

# Methods ---------------------------------------------------------------------

#' @S3method compute tbl_sqlite
compute.tbl_sqlite <- function(x, name = random_table_name(), ...) {
  qry_select(x)$save_into(name)
  tbl(x$src, name)
}

#' @S3method collect tbl_sqlite
collect.tbl_sqlite <- function(x, ...) {
  tbl_df(qry_select(x)$fetch_df())
}

#' @S3method compute tbl_df
compute.tbl_df <- function(x, ...) x
#' @S3method compute tbl_dt
compute.tbl_dt <- function(x, ...) x
#' @S3method collect tbl_df
collect.tbl_df <- function(x, ...) x
#' @S3method collect tbl_dt
collect.tbl_dt <- function(x, ...) x
