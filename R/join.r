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
    if (!identical(data.table::key(x),by$x)){
      if (!setkey) x <- copy(x)
      data.table::setkeyv(x, by$x)
    }
    if (!identical(data:table::key(y),by$y)){
      if (!setkey) y <- copy(y)
      data.table::setkeyv(y, by$y)
    }
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
