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
#' batting <- select(tbl(lahman(), "Batting"), playerID:lgID, G_batting)
#' pitching <- select(filter(tbl(lahman(), "Fielding"), POS == "P"), playerID:lgID, GS)
#'
#' both <- compound(batting, pitching)
#' hou <- filter(both, teamID == "HOU")
#' explain_tbl(hou)
#' head(hou)
compound.tbl_sqlite <- function(x, y) {
#   type <- match.arg(tolower(type), 
#     c("union all", "union", "intersect", "exclude"))
 
  y <- auto_copy(x, y, copy, indexes = if (auto_index) list(by))

  if (!is.null(x$arrange) || !is.null(y$arrange)) {
    stop("tbls used in a compound select can not be ordered", call. = FALSE)
  }
  
  from <- build_sql("(", x$query$sql, " UNION ALL ", y$query$sql, ")")
  tbl(x$src, from)
}

