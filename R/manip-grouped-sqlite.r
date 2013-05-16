#' Data manipulation for grouped SQL data sources.
#'
#' @param .data an SQLite data base
#' @param ... variables interpreted in the context of \code{.data}
#' @param .n maximum number of columns to return. Set to \code{-1} to return
#'  all.
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- source_sqlite(db_path, "baseball")
#' players <- group_by(baseball_s, id)
#'
#' # Due to the lack of windowing functions in SQLite, only summarising
#' # is actually useful
#' summarise(players, g = mean(g))
#' summarise(players, g = mean(g), best_ab = max(ab))
#'
#' per_year <- group_by(baseball_s, id, year)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#'
#' # All other operations will ignore grouping, although they will preserve it
#' # in the object returned to R.
#' filter(players, g > 100)
#' mutate(players, rbi = 1 * r / ab)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # NB: If you use an aggregation function, you will get one row:
#' mutate(players, cyear = year - min(year) + 1)
#'
#' @name manip_grouped_sqlite
NULL

#' @rdname manip_grouped_sqlite
#' @export
#' @method filter grouped_sqlite
filter.grouped_sqlite <- function(.data, ...) {
  warning("Group by ignored for filtering with SQLite", call. = FALSE)

  filter.source_sqlite(.data, ...)
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method summarise grouped_sqlite
summarise.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  select <- translate_sql_q(dots(...), .data, parent.frame())
  group_by <- to_sql(.data$group_by)

  out <- sql_select(.data,
    select = c(group_by, select),
    group_by = names(group_by),
    n = .n)

  source_df(
    data = out
  )
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method mutate grouped_sqlite
mutate.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  warning("Group by ignored for mutate with SQLite", call. = FALSE)

  out <- mutate.source_sqlite(.data, ..., .n = .n)
  grouped_df(
    data = out,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method arrange grouped_sqlite
arrange.grouped_sqlite <- function(.data, ...) {
  warning("Group by ignored for arrange with SQLite", call. = FALSE)

  arrange.source_sqlite(.data, ...)
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method select grouped_sqlite
select.grouped_sqlite <- function(.data, ...) {
  warning("Group by ignored for select with SQLite", call. = FALSE)

  select.source_sqlite(.data, ...)
}
