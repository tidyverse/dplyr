#' Compute a lazy tbl.
#'
#' \code{compute} forces computation of lazy tbls, leaving data in the remote
#' source. \code{collect} also forces computation, but will bring data back into
#' an R data.frame (stored in a \code{\link{tbl_df}}). \code{collapse} doesn't
#' force computation, but collapses a complex tbl into a form that additional
#' restrictions can be placed on.
#'
#' @section Grouping:
#'
#' \code{compute} and \code{collect} preserve grouping, \code{collapse} drops
#' it.
#'
#' @param x a data tbl
#' @param name name of temporary table on database.
#' @param ... other arguments passed on to methods
#' @seealso \code{\link{copy_to}} which is the conceptual opposite: it
#'   takes a local data frame and makes it available to the remote source.
#' @export
#' @examples
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   remote <- select(filter(batting, yearID > 2010 && stint == 1), playerID:H)
#'   remote2 <- collapse(remote)
#'   cached <- compute(remote)
#'   local  <- collect(remote)
#' }
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

#' @export
collapse.tbl_sql <- function(x, vars = NULL, ...) {
  # If you collapse a query, the names of the fields will be the output names
  # of the previous query.
  if (is.null(vars)) {
    nms <- auto_names(x$select)
    vars <- lapply(nms, as.name)
  }

  update(tbl(x$src, x$query$sql, vars = vars, ...), group_by = groups(x))
}

#' @export
compute.tbl_sql <- function(x, name = random_table_name(), ...) {
  x$query$save_into(name)
  update(tbl(x$src, name), group_by = groups(x))
}

#' @export
collect.tbl_sql <- function(x, ...) {
  grouped_df(x$query$fetch(), groups(x))
}


#' @export
collapse.tbl_df <- function(x, ...) x
#' @export
collapse.tbl_dt <- function(x, ...) x

#' @export
compute.tbl_df <- function(x, ...) x
#' @export
compute.tbl_dt <- function(x, ...) x

#' @export
collect.tbl_df <- function(x, ...) x
#' @export
collect.tbl_dt <- function(x, ...) x
