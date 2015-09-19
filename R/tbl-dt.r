#' Create a data table tbl.
#'
#' A data table tbl wraps a local data table.
#'
#' @export
#' @param data a data table
#' @param copy If the input is a data.table, copy it?
#' @aliases .datatable.aware
#' @examples
#' if (require("data.table")) {
#' ds <- tbl_dt(mtcars)
#' ds
#' as.data.table(ds)
#' as.tbl(mtcars)
#' }
#'
#' if (require("data.table") && require("nycflights13")) {
#' flights2 <- tbl_dt(flights)
#' flights2 %>% filter(month == 1, day == 1, dest == "DFW")
#' flights2 %>% select(year:day)
#' flights2 %>% rename(Year = year)
#' flights2 %>%
#'   summarise(
#'     delay = mean(arr_delay, na.rm = TRUE),
#'     n = length(arr_delay)
#'   )
#' flights2 %>%
#'   mutate(gained = arr_delay - dep_delay) %>%
#'   select(ends_with("delay"), gained)
#' flights2 %>%
#'   arrange(dest, desc(arr_delay))
#'
#' by_dest <- group_by(flights2, dest)
#'
#' filter(by_dest, arr_delay == max(arr_delay, na.rm = TRUE))
#' summarise(by_dest, arr = mean(arr_delay, na.rm = TRUE))
#'
#' # Normalise arrival and departure delays by airport
#' by_dest %>%
#'   mutate(arr_z = scale(arr_delay), dep_z = scale(dep_delay)) %>%
#'   select(starts_with("arr"), starts_with("dep"))
#'
#' arrange(by_dest, desc(arr_delay))
#' select(by_dest, -(day:tailnum))
#' rename(by_dest, Year = year)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(flights2, year, month, day)
#' by_month <- summarise(by_day, delayed = sum(arr_delay > 0, na.rm = TRUE))
#' by_month
#' summarise(by_month, delayed = sum(delayed))
#'
#' # You can also manually ungroup:
#' ungroup(by_day)
#' }
tbl_dt <- function(data, copy = TRUE) {
  if (!requireNamespace("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }
  if (is.grouped_dt(data)) return(ungroup(data))

  if (data.table::is.data.table(data)) {
    if (copy)
      data <- data.table::copy(data)
  } else {
    data <- data.table::as.data.table(data)
  }
  data.table::setattr(data, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))
  data
}

#' @export
as.tbl.data.table <- function(x, ...) {
  tbl_dt(x)
}

#' @export
tbl_vars.tbl_dt <- function(x) data.table::copy(names(x))

#' @export
groups.tbl_dt <- function(x) {
  NULL
}

#' @export
ungroup.tbl_dt <- function(x) x

#' @export
ungroup.data.table <- function(x) x

#' @export
same_src.tbl_dt <- function(x, y) {
  data.table::is.data.table(y)
}

#' @export
auto_copy.tbl_dt <- function(x, y, copy = FALSE, ...) {
  data.table::as.data.table(as.data.frame(y))
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
#   if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
#   if (!identical(optional, FALSE)) warning("optional argument ignored", call. = FALSE)
  NextMethod()
}

#' @export
#' @rdname dplyr-formatting
print.tbl_dt <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data table ", dim_desc(x), "\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
dimnames.tbl_dt <- function(x) data.table::copy(NextMethod())

#' @export
head.tbl_dt <- function(x, ...) as.data.frame(NextMethod())

#' @export
tail.tbl_dt <- function(x, ...) tbl_df(as.data.frame(NextMethod()))

#' @export
.datatable.aware <- TRUE

# Filter -----------------------------------------------------------------------

and_expr <- function(exprs) {
  assert_that(is.list(exprs))
  if (length(exprs) == 0) return(TRUE)
  if (length(exprs) == 1) return(exprs[[1]])

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

#' @export
filter_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
filter_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
filter_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  env <- lazyeval::common_env(dots)

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  expr <- lapply(dots, `[[`, "expr")
  j <- substitute(list(`_row` = .I[expr]), list(expr = and_expr(expr)))
  indices <- dt_subset(.data, , j, env)$`_row`

  .data[indices[!is.na(indices)]]
}

# Summarise --------------------------------------------------------------------

#' @export
summarise_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
summarise_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
summarise_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  j <- lazyeval::make_call(quote(list), dots)
  dt_subset(.data, , j$expr, env = j$env)
}

# Mutate -----------------------------------------------------------------------

#' @export
mutate_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}

#' @export
mutate_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
mutate_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  names <- lapply(names(dots), as.name)

  # Never want to modify in place
  .data <- data.table::copy(.data)

  for(i in seq_along(dots)) {
    # For each new variable, generate a call of the form df[, new := expr]
    j <- substitute(lhs := rhs, list(lhs = names[[i]], rhs = dots[[i]]$expr))
    .data <- dt_subset(.data, , j, dots[[i]]$env)
  }

  .data
}

# Arrange ----------------------------------------------------------------------

#' @export
arrange_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
arrange_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
arrange_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  groups <- lazyeval::as.lazy_dots(groups(.data),
    env = lazyeval::common_env(dots))
  i <- lazyeval::make_call(quote(order), c(groups, dots))

  dt_subset(.data, i$expr, , env = i$env)
}

# Select -----------------------------------------------------------------------

#' @export
select_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots,
    include = as.character(groups(.data)))
  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
select_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
select_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Rename -----------------------------------------------------------------------

#' @export
rename_.grouped_dt <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}

#' @export
rename_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)

  out <- .data[, vars, drop = FALSE, with = FALSE]
  data.table::setnames(out, names(vars))
  out
}

#' @export
rename_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}


# Slice -------------------------------------------------------------------

#' @export
slice_.grouped_dt <- function(.data, ..., .dots) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}

#' @export
slice_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @export
slice_.data.table <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  env <- lazyeval::common_env(dots)

  j <- substitute(.SD[rows], list(rows = dots[[1]]$expr))
  dt_subset(.data, , j, env)
}

# Do ---------------------------------------------------------------------------

#' @export
do_.data.table <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}

#' @export
do_.tbl_dt <- function(.data, .f, ...) {
  list(.f(as.data.frame(.data), ...))
}


# Joins ------------------------------------------------------------------------

#' Join data table tbls.
#'
#' See \code{\link{join}} for a description of the general purpose of the
#' functions.
#'
#' @inheritParams join
#' @param x,y tbls to join
#' @param ... Included for compatibility with generic; otherwise ignored.
#' @examples
#' if (require("data.table") && require("Lahman")) {
#' batting_dt <- tbl_dt(Batting)
#' person_dt <- tbl_dt(Master)
#'
#' # Inner join: match batting and person data
#' inner_join(batting_dt, person_dt)
#'
#' # Left join: keep batting data even if person missing
#' left_join(batting_dt, person_dt)
#'
#' # Semi-join: find batting data for top 4 teams, 2010:2012
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"),
#'   yearID = 2010:2012)
#' top4 <- semi_join(batting_dt, grid, copy = TRUE)
#'
#' # Anti-join: find batting data with out player data
#' anti_join(batting_dt, person_dt)
#' }
#' @name join.tbl_dt
NULL

join_dt <- function(op) {
  template <- substitute(function(x, y, by = NULL, copy = FALSE, ...) {
    by <- common_by(by, x, y)
    if (!identical(by$x, by$y)) {
      stop("Data table joins must be on same key", call. = FALSE)
    }
    y <- auto_copy(x, y, copy = copy)

    x <- data.table::copy(x)
    y <- data.table::copy(y)
    data.table::setkeyv(x, by$x)
    data.table::setkeyv(y, by$x)
    out <- op
    grouped_dt(out, groups(x))
  })

  f <- eval(template, parent.frame())
  attr(f, "srcref") <- NULL # fix so prints correctly
  f
}

#' @export
#' @rdname join.tbl_dt
inner_join.data.table <- join_dt(merge(x, y, by = by$x, allow.cartesian = TRUE))

#' @export
#' @rdname join.tbl_dt
left_join.data.table  <- join_dt(merge(x, y, by = by$x, all.x = TRUE, allow.cartesian = TRUE))

#' @export
#' @rdname join.tbl_dt
semi_join.data.table  <- join_dt({
  # http://stackoverflow.com/questions/18969420/perform-a-semi-join-with-data-table
  w <- unique(x[y, which = TRUE, allow.cartesian = TRUE])
  w <- w[!is.na(w)]
  x[w]
})

#' @export
#' @rdname join.tbl_dt
anti_join.data.table <- join_dt(x[!y, allow.cartesian = TRUE])

# Set operations ---------------------------------------------------------------

#' @export
distinct_.data.table <- function(.data, ..., .dots) {
  dist <- distinct_vars(.data, ..., .dots = .dots)

  if (length(dist$vars) == 0) {
    unique(dist$data)
  } else {
    unique(dist$data, by = dist$vars)
  }
}

#' @export
distinct_.tbl_dt <- function(.data, ..., .dots) {
  tbl_dt(NextMethod(), copy = FALSE)
}

