#' Data manipulation for SQL data sources.
#'
#' @examples
#' data("baseball", package = "plyr")
#' baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
#
#' filter(baseball_s, year > 2005, g > 130)
#' head(select(baseball_s, id:team))
#' summarise(baseball_s, g = mean(g), n = count())
#' head(mutate(baseball_s, rbi = 1.0 * r / ab))
#' head(arrange(baseball_s, id, desc(year)))
#'
#' @name manip_sqlite
NULL

#' @rdname manip_sqlite
#' @export
#' @method filter source_sqlite
filter.source_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  where <- translate_sql_q(dots(...), .data, parent.frame())
  sql_select(.data, "*", where = where, n = .n)
}

#' @rdname manip_sqlite
#' @export
#' @method summarise source_sqlite
summarise.source_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  select <- translate_sql_q(dots(...), .data, parent.frame())
  sql_select(.data, select = select, n = .n)
}

#' @rdname manip_sqlite
#' @export
#' @method mutate source_sqlite
mutate.source_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  select <- translate_sql_q(dots(...), .data, parent.frame())
  sql_select(.data, select = c("*", select), n = .n)
}

#' @rdname manip_sqlite
#' @export
#' @method arrange source_sqlite
arrange.source_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  order_by <- translate_sql_q(dots(...), .data, parent.frame())
  sql_select(.data, "*", order_by = order_by, n = .n)
}

#' @rdname manip_sqlite
#' @export
#' @method select source_sqlite
select.source_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  nm <- names(.data)
  nm_env <- as.list(setNames(seq_along(nm), nm))

  idx <- unlist(lapply(dots(...), eval, nm_env, parent.frame()))
  select <- nm[idx]

  sql_select(.data, select, n = .n)
}
