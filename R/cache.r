#' Cache a compound tbl.
#' 
#' This precomputes a compound tbl and saves it (if possible) in the remote src.
#' It's like \code{as.data.frame} but instead of bringing all the data back 
#' locally, it saves it remotely. For sql datasources, it will create a 
#' temporary table.
#' 
#' @param x a data tbl
#' @param name name to use for temporary table. Defaults to a random name.
#' @param ... other arguments passed on to methods
#' @export
#' @examples
#' lah <- lahman()
#' batting <- select(filter(tbl(lah, "Batting"), stint == 1), playerID:H)
#' team <- select(tbl(lah, "Teams"), yearID, lgID, teamID, G, R:H)
#' both <- join(batting, team, type = "inner", by = c("yearID", "teamID", "lgID"))
#' hou <- filter(both, teamID == "HOU")
#' hou <- cache(hou)
cache <- function(x, name = random_table_name(), ...) {
  UseMethod("cache")
}

#' @S3method cache tbl_sqlite
cache.tbl_sqlite <- function(x, name = random_table_name(), ...) {
  sql_select(x, into_table = name)
  
  tbl(x$src, name)
}
