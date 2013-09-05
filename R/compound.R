#' Create a compound select.
#' 
#' Combine together two tables.
#' 
#' @param x,y \code{tbl}s to combine
#' @param ... other parameters passed to individual methods
compound <- function(x, y, ...) {
  UseMethod("compound")
}

#' Create a compound sqlite tbl
#' 
#' @method compound tbl_sqlite
#' @export
#' @examples
#' l <- lahman()
#' batting <- select(tbl(l, "Batting"), playerID:lgID, G_batting)
#' pitching <- select(filter(tbl(l, "Fielding"), POS == "P"), playerID:lgID, GS)
#'
#' both <- compound(batting, pitching)
#' hou <- filter(plays, teamID == "HOU")
#' head(hou)
compound.tbl_sqlite <- function(x, y) {
#   type <- match.arg(tolower(type), 
#     c("union all", "union", "intersect", "exclude"))
 
  if (!same_src(x, y)) {
    stop("x and y must share the same source", call. = FALSE)    
  }
  if (!is.null(x$arrange) || !is.null(y$arrange)) {
    stop("tbls used in a compound select can not be ordered", call. = FALSE)
  }
  
  from <- build_sql("(", select_qry(x)$sql, " UNION ALL ", select_qry(y)$sql, ")")
  tbl(src, from)
}

