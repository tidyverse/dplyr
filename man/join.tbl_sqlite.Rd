\name{join.tbl_sqlite}
\alias{join.tbl_sqlite}
\title{Join SQLite tbls.}
\usage{
  \method{join}{tbl_sqlite} (x, y, by = NULL,
    type = "left", copy = FALSE, auto_index = FALSE, ...)
}
\arguments{
  \item{copy}{If \code{x} and \code{y} are not from the
  same data source, and \code{copy} is \code{TRUE}, then
  \code{y} will be copied into a temporary table in same
  database as \code{x}. \code{join} will automatically run
  \code{ANALYSE} on the created table in the hope that this
  will make you queries as efficient as possible by giving
  more data to the query planner.

  This allows you to join tables across srcs, but it's
  potentially expensive operation so you must opt into it.}

  \item{auto_index}{if \code{copy} is \code{TRUE},
  automatically create indices for the variables in
  \code{by}. This may speed up the join if there are
  matching indices in \code{x}.}

  \item{by}{a character vector of variables to join by}

  \item{type}{a string giving the join type. Possible
  values are left (the default), right, full and inner. Not
  all types will be supported by all tbls.}

  \item{...}{other parameters passed onto individual
  methods}
}
\description{
  Join SQLite tbls.
}
\examples{
batting <- tbl(lahman(), "Batting")
team_info <- select(tbl(lahman(), "Teams"), yearID, lgID, teamID, G, R:H)

# Combine player and whole team statistics
first_stint <- select(filter(batting, stint == 1), playerID:H)
both <- join(first_stint, team_info, type = "inner", by = c("yearID", "teamID", "lgID"))
head(both)
explain_tbl(both)

# Join with a local data frame
grid <- expand.grid(
  teamID = c("WAS", "ATL", "PHI", "NYA"),
  yearID = 2010:2012)
top4a <- join(batting, grid, copy = TRUE, type = "inner")
explain_tbl(top4a)

# Indices don't really help here because there's no matching index on
# batting
top4b <- join(batting, grid, copy = TRUE, type = "inner", auto_index = TRUE)
explain_tbl(top4b)
}

