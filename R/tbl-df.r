#' Create a data frame tbl.
#'
#' A data frame tbl wraps a local data frame. The main advantage to using
#' a \code{tbl_df} over a regular data frame is the printing:
#' tbl objects only print a few rows and all the columns that fit on one
#' screen, describing the rest of it as text.
#'
#' @section Methods:
#'
#' \code{tbl_df} implements two important base methods:
#'
#' \describe{
#' \item{print}{Only prints the first 10 rows, and the columns that fit on
#'   screen}
#' \item{\code{[}}{Never simplifies (drops), so always returns data.frame}
#' }
#'
#' @export
#' @param data a data frame
#' @examples
#' ds <- tbl_df(mtcars)
#' ds
#' as.data.frame(ds)
#'
#' if (require("Lahman") && packageVersion("Lahman") >= "3.0.1") {
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
#' best_year <- filter(players, AB == max(AB) | G == max(G))
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
#' player_info <- select(tbl_df(Master), playerID, birthYear)
#' hof <- select(filter(tbl_df(HallOfFame), inducted == "Y"),
#'  playerID, votedBy, category)
#'
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # Find players not in hof
#' anti_join(player_info, hof)
#' }
tbl_df <- function(data) {
  assert_that(is.data.frame(data))
  as_data_frame(data)
}

#' @export
as.tbl.data.frame <- function(x, ...) {
  tbl_df(x)
}

#' @export
tbl_vars.data.frame <- function(x) names(x)

#' @export
same_src.data.frame <- function(x, y) {
  is.data.frame(y)
}

# Grouping methods ------------------------------------------------------------

# These are all inherited from data.frame - see tbl-data-frame.R

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_df <- function(x, row.names = NULL, optional = FALSE, ...) {
  as_regular_df(x)
}

#' @rdname dplyr-formatting
#' @export
print.tbl_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
`[.tbl_df` <- function (x, i, j, drop = FALSE) {
  if (missing(i) && missing(j)) return(x)
  if (drop) warning("drop ignored", call. = FALSE)

  nr <- nrow(x)

  # Escape early if nargs() == 2L; ie, column subsetting
  if (nargs() == 2L) {
    result <- .subset(x, i)
    class(result) <- c("tbl_df", "data.frame")
    attr(result, "row.names") <- .set_row_names(nr)
    return(result)
  }

  # First, subset columns
  if (!missing(j)) {
    x <- .subset(x, j)
  }

  # Next, subset rows
  if (!missing(i)) {
    if (length(x) == 0) {
      nr <- length(attr(x, "row.names")[i])
    } else {
      x <- lapply(x, `[`, i)
      nr <- length(x[[1]])
    }
  }

  class(x) <- c("tbl_df", "data.frame")
  attr(x, "row.names") <- .set_row_names(nr)
  x
}

