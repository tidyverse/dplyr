#' Compute a lazy tbl.
#'
#' `compute()` forces computation of lazy tbls, leaving data in the remote
#' source. `collect()` also forces computation, but will bring data back into
#' an R data.frame (stored in a [tbl_df()]). `collapse()` doesn't
#' force computation, but collapses a complex tbl into a form that additional
#' restrictions can be placed on.
#'
#' @section Grouping:
#'
#' `compute()`, `collect()`, and `collapse()` preserve grouping.
#'
#' @param x a data tbl
#' @param name name of temporary table on database.
#' @param ... other arguments passed on to methods
#' @inheritParams copy_to.src_sql
#' @seealso [copy_to()] which is the conceptual opposite: it
#'   takes a local data frame and makes it available to the remote source.
#' @export
#' @examples
#' \donttest{
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   remote <- select(filter(batting, yearID > 2010 && stint == 1), playerID:H)
#'   remote2 <- collapse(remote)
#'   cached <- compute(remote)
#'   local  <- collect(remote)
#' }
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
