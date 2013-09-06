\name{semi_join.tbl_sqlite}
\alias{semi_join.tbl_sqlite}
\title{Semi-join for SQLite tbls.}
\usage{
  semi_join.tbl_sqlite(x, y, by = NULL, copy = FALSE,
    auto_index = FALSE, ...)
}
\arguments{
  \item{by}{a character vector of variables to join by.  If
  \code{NULL}, the default, \code{join} will do a natural
  join, using all variables with common names across the
  two tables. A message lists the variables so that you can
  check they're right.}

  \item{copy}{If \code{x} and \code{y} are not from the
  same data source, and \code{copy} is \code{TRUE}, then
  \code{y} will be copied into the same src as \code{x}.
  This allows you to join tables across srcs, but it is a
  potentially expensive operation so you must opt into it.}

  \item{...}{other parameters passed onto individual
  methods}
}
\description{
  Semi-join for SQLite tbls.
}
\examples{
people <- tbl(lahman(), "Master")

# Find all people in half of fame
hof <- tbl(lahman(), "HallOfFame")
semi_join(people, hof)

# Find all managers
manager <- tbl(lahman(), "Managers")
semi_join(people, manager)

# Find all managers in hall of fame
semi_join(semi_join(people, manager), hof)
}

