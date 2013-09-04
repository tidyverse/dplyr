#' Create a compound select.
#' 
#' Combine together the multiple tables.
#' 
#' @param ... \code{tbl}s to combine
#' @param type type of combination.
#' @examples
#' l <- lahman()
#' batting <- select(tbl(l, "Batting"), playerID:lgID, G_batting)
#' pitching <- select(filter(tbl(l, "Fielding"), POS == "P"), playerID:lgID, GS)
#' outfield <- select(filter(tbl(l, "Fielding"), POS == "OF"), playerID:lgID, GS)
#'
#' plays <- compound(batting, pitching, outfield)
#' hou <- filter(plays, teamID == "HOU")
#' head(hou)
compound <- function(...) {
  UseMethod("compound")
}

compound.tbl_sqlite <- function(...) {
#   type <- match.arg(tolower(type), 
#     c("union all", "union", "intersect", "exclude"))
 
  tbls <- list(...)
  src <- tbls[[1]]$src
  for (tbl in tbls) {
    if (!is.null(tbl$arrange)) {
      stop("tbls used in a compound select can not be ordered", call. = FALSE)
    }
    
    if (!identical(src$con, tbl$src$con)) {
      stop("All tbls must come from the same source", call. = FALSE)
    }
  }
  
  from <- do.call("c", lapply(tbls, function(x) select_qry(x)$sql))
  selects <- escape(from, collapse = " UNION ALL ", parens = TRUE)
  
  tbl(src, selects)
}

