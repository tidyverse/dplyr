#' Create a data frame tble.
#'
#' A data frame tbl wraps a local data frame. The main advantage to using
#' a \code{tbl_df} over a regular data frame is the printing:
#' tbl objects only print a few rows and all the columns that fit on one
#' screen, providing describing the rest of it as text.
#'
#' @export
#' @param data a data frame
#' @examples
#' ds <- tbl_df(mtcars)
#' ds
#' as.data.frame(ds)
#' 
#' library(Lahman)
#' batting <- tbl_df(Batting)
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)
#' 
#' # Group by operations -------------------------------------------------------
#' # To perform operations by group, create a grouped object with group_by
#' players <- group_by(batting, playerID)
#' head(group_size(players), 100)
#'
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#' best_year <- filter(players, AB == max(AB) || G == max(G))
#' progress <- mutate(players, cyear = yearID - min(yearID) + 1, 
#'  rank(desc(AB)), cumsum(AB))
#'  
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' # filter(stints, stints > 3)
#' # summarise(stints, max(stints))
#' # mutate(stints, cumsum(stints))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl_df(Master), playerID, hofID, birthYear)
#' hof <- select(filter(tbl_df(HallOfFame), inducted == "Y"),
#'  hofID, votedBy, category)
#' 
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # Find players not in hof
#' anti_join(player_info, hof)
tbl_df <- function(data) {
  assert_that(is.data.frame(data))
  if (is.grouped_cpp(data)) return(ungroup(data))
  
  class(data) <- c("tbl_cpp", "tbl", "data.frame")
  data
}

#' @S3method as.tbl data.frame
as.tbl.data.frame <- function(x, ...) {
  tbl_df(x)
}

#' @S3method tbl_vars data.frame
tbl_vars.data.frame <- function(x) names(x)

#' @S3method same_src data.frame
same_src.data.frame <- function(x, y) {
  is.data.frame(y)
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_cpp <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
print.tbl_cpp <- function(x, ...) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x)
}

