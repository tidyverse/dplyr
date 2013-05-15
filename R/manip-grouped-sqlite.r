#' Data manipulation for grouped SQL data sources.
#'
#' @examples
#' baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
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
filter.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  warning("Group by ignored for filtering with SQLite", call. = FALSE)

  out <- filter.source_sqlite(.data, ..., .n = .n)
  grouped_data_frame(
    data = out,
    name = .data$table,
    vars = .data$vars
  )
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

  data_frame_source(
    data = out,
    name = .data$table
  )
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method mutate grouped_sqlite
mutate.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  warning("Group by ignored for mutate with SQLite", call. = FALSE)

  out <- mutate.source_sqlite(.data, ..., .n = .n)
  grouped_data_frame(
    data = out,
    name = .data$table,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method arrange grouped_sqlite
arrange.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  warning("Group by ignored for arrange with SQLite", call. = FALSE)

  out <- arrange.source_sqlite(.data, ..., .n = .n)
  grouped_data_frame(
    data = out,
    name = .data$table,
    vars = .data$vars
  )
}

#' @rdname manip_grouped_sqlite
#' @export
#' @method select grouped_sqlite
select.grouped_sqlite <- function(.data, ..., .n = 1e5) {
  warning("Group by ignored for select with SQLite", call. = FALSE)

  out <- select.source_sqlite(.data, ..., .n = .n)
  grouped_data_frame(
    data = out,
    name = .data$table,
    vars = .data$vars
  )
}
