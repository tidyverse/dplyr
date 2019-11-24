#' Force computation of a database query
#'
#' `compute()` stores results in a remote temporary table.
#' `collect()` retrieves data into a local tibble.
#' `collapse()` is slightly different: it doesn't force computation, but
#' instead forces generation of the SQL query. This is sometimes needed to work
#' around bugs in dplyr's SQL generation.
#'
#' All functions preserve grouping and ordering.
#'
#' @param x A tbl
#' @param name Name of temporary table on database.
#' @param ... Other arguments passed on to methods
#' @seealso [copy_to()], the opposite of `collect()`: it takes a local data
#'   frame and uploads it to the remote source.
#' @export
#' @examples
#' if (require(dbplyr)) {
#'   mtcars2 <- src_memdb() %>%
#'     copy_to(mtcars, name = "mtcars2-cc", overwrite = TRUE)
#'
#'   remote <- mtcars2 %>%
#'     filter(cyl == 8) %>%
#'     select(mpg:drat)
#'
#'   # Compute query and save in remote table
#'   compute(remote)
#'
#'   # Compute query bring back to this session
#'   collect(remote)
#'
#'   # Creates a fresh query based on the generated SQL
#'   collapse(remote)
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
