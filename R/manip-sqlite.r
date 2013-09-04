#' Data manipulation for SQL tbls.
#'
#' Arrange, filter and select are lazy: they modify the object representing
#' the table, and do not recompute unless needed.  Summarise and mutate
#' are eager: they will always return a tbl_df.
#'
#' @param .data an SQLite data base
#' @param ... variables interpreted in the context of \code{.data}
#' @param .n maximum number of columns to return. Set to \code{-1} to return
#'  all.
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- tbl_sqlite(db_path, "baseball")
#'
#' # filter, select and arrange lazily modify the specification of the table
#' # they don't execute queries unless you print them
#' filter(baseball_s, year > 2005, g > 130)
#' select(baseball_s, id:team)
#' arrange(baseball_s, id, desc(year))
#'
#' # summarise and mutate always return data frame tbls
#' summarise(baseball_s, g = mean(g), n = count())
#' mutate(baseball_s, rbi = 1.0 * r / ab)
#'
#' @name manip_sqlite
NULL

#' @rdname manip_sqlite
#' @export
#' @method filter tbl_sqlite
filter.tbl_sqlite <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$filter <- c(.data$filter, input)
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method arrange tbl_sqlite
arrange.tbl_sqlite <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$arrange <- c(.data$arrange, input)
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method select tbl_sqlite
select.tbl_sqlite <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  .data$select <- ident(c(.data$select, input))
  .data
}

#' @rdname manip_sqlite
#' @export
#' @method summarise tbl_sqlite
summarise.tbl_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)
  if (!is.null(.data$select)) {
    warning("Summarise ignores selected variables", call. = FALSE)
  }

  select <- trans_sqlite(dots(...), .data, parent.frame())
  out <- sql_select(.data, select = select, n = .n)
  tbl_df(
    data = out
  )
}

#' @rdname manip_sqlite
#' @export
#' @method mutate tbl_sqlite
mutate.tbl_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  old_vars <- .data$select %||% "*"
  new_vars <- trans_sqlite(dots(...), .data, parent.frame())

  out <- sql_select(.data, select = c(old_vars, new_vars), n = .n)
  tbl_df(
    data = out
  )
}
