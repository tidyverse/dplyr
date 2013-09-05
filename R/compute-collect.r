#' Compute a lazy tbl.
#' 
#' \code{compute} forces computation of lazy tbls, leaving data in the remote
#' source. \code{collect} also forces computation, but will bring data back into
#' an R data.frame (stored in a \code{\link{tbl_df}}). \code{collapse} doesn't
#' force computation, but collapses a complex tbl into a form that additional
#' restrictions can be placed on.
#' 
#' @param x a data tbl
#' @param ... other arguments passed on to methods
#' @seealso \code{\link{copy_to}} which is the conceptual opposite: it 
#'   takes a local data frame and makes it available to the remote source.
#' @export
#' @examples
#' batting <- tbl(lahman(), "Batting")
#' remote <- select(filter(batting, year > 2010 && stint == 1), playerID:H)
#' remote2 <- collapse(remote)
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

#' @export
#' @rdname compute
collapse <- function(x, ...) {
  UseMethod("collapse")
}

# Methods ---------------------------------------------------------------------

#' @S3method collapse tbl_sqlite
collapse.tbl_sqlite <- function(x, ...) {
  tbl(x$src, qry_select(x)$sql)
}

#' @S3method compute tbl_sqlite
compute.tbl_sqlite <- function(x, name = random_table_name(), ...) {
  qry_select(x)$save_into(name)
  tbl(x$src, name)
}

#' @S3method collect tbl_sqlite
collect.tbl_sqlite <- function(x, ...) {
  tbl_df(qry_select(x)$fetch_df())
}


#' @S3method collapse tbl_df
collapse.tbl_df <- function(x, ...) x
#' @S3method collapse tbl_dt
collapse.tbl_dt <- function(x, ...) x
#' @S3method compute tbl_df
compute.tbl_df <- function(x, ...) x
#' @S3method compute tbl_dt
compute.tbl_dt <- function(x, ...) x
#' @S3method collect tbl_df
collect.tbl_df <- function(x, ...) x
#' @S3method collect tbl_dt
collect.tbl_dt <- function(x, ...) x
