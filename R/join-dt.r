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
  template <- substitute(function(x, y, by = NULL, copy = FALSE, setkey = FALSE, ...) {
    by <- common_by(by, x, y)
    
    y <- auto_copy(x, y, copy = copy)
    if (!identical(data.table::key(x),by$x)){
      if (!setkey) x <- copy(x)
      data.table::setkeyv(x, by$x)
    }
    if (!identical(data:table::key(y),by$y)){
      if (!setkey) y <- copy(y)
      data.table::setkeyv(y, by$y)
    }
    

    # Accept different names
    # In future versions, the command copy below may be replaced by shallow copies.
    
    # first output error if there are variables in y-by$y with the same name than variables in by$x 
    # This behavior is different from dplyr behavior for data.frame that does not output error
    # Needed because merge.data.table does not accept names duplicates in master/using data.tables
    names_byx_y <- intersect(by$x, setdiff(names(y), by$y))
    if (length(names_byx_y) >0) stop(paste(names_byx_y,"is a variable to be matched in x and not in y. Please rename",names_byx_y, "in x or y"))
    if (!identical(by$x,by$y)){
      y <- copy(y)
      data.table::setnames(y, by$y, by$x)
    }
    # Rename duplicates in non joined variables
    common_names <- setdiff(intersect(names(x), names(y)), by$x)
    if (length(common_names)>0){
      x <- copy(x)
      y <- copy(y)
      data.table::setnames(x, common_names, paste0(common_names, ".x"))
      data.table::setnames(y, common_names, paste0(common_names, ".y")) 
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
