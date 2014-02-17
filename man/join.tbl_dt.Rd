\name{join.tbl_dt}
\alias{anti_join.tbl_dt}
\alias{inner_join.tbl_dt}
\alias{join.tbl_dt}
\alias{left_join.tbl_dt}
\alias{semi_join.tbl_dt}
\title{Join data table tbls.}
\usage{
\method{inner_join}{tbl_dt}(x, y, by = NULL, copy = FALSE, ...)

\method{left_join}{tbl_dt}(x, y, by = NULL, copy = FALSE, ...)

\method{semi_join}{tbl_dt}(x, y, by = NULL, copy = FALSE, ...)

\method{anti_join}{tbl_dt}(x, y, by = NULL, copy = FALSE, ...)
}
\arguments{
  \item{x,y}{tbls to join}

  \item{by}{a character vector of variables to join by.  If
  \code{NULL}, the default, \code{join} will do a natural
  join, using all variables with common names across the
  two tables. A message lists the variables so that you can
  check they're right - to suppress the message, supply a
  character vector.}

  \item{copy}{If \code{y} is not a data table or
  \code{\link{tbl_dt}} and \code{copy} is \code{TRUE},
  \code{y} will be converted into a data table.}

  \item{...}{Included for compatibility with generic;
  otherwise ignored.}
}
\description{
See \code{\link{join}} for a description of the general
purpose of the functions.
}
\examples{
if (require("RSQLite") && require("RSQLite.extfuns")) {
data("Batting", package = "Lahman")
data("Master", package = "Lahman")

batting_dt <- tbl_dt(Batting)
person_dt <- tbl_dt(Master)

# Inner join: match batting and person data
inner_join(batting_dt, person_dt)

# Left join: keep batting data even if person missing
left_join(batting_dt, person_dt)

# Semi-join: find batting data for top 4 teams, 2010:2012
grid <- expand.grid(
  teamID = c("WAS", "ATL", "PHI", "NYA"),
  yearID = 2010:2012)
top4 <- semi_join(batting_dt, grid, copy = TRUE)

# Anti-join: find batting data with out player data
anti_join(batting_dt, person_dt)
}
}

