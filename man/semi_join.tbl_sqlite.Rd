\name{semi_join.tbl_sqlite}
\alias{semi_join.tbl_sqlite}
\title{Semi-join for SQLite tbls.}
\usage{
  semi_join.tbl_sqlite(x, y, by = NULL, anti = FALSE,
    copy = FALSE, auto_index = FALSE, ...)
}
\arguments{
  \item{anti}{If \code{TRUE}, performs an anti join instead
  of a semi join.  Anti joins use \code{WHERE NOT EXISTS}
  rather than \code{WHERE EXISTS}}

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

  \item{...}{other parameters passed onto methods}

  \item{auto_index}{if \code{copy} is \code{TRUE},
  automatically create indices for the variables in
  \code{by}. This may speed up the join if there are
  matching indices in \code{x}.}
}
\description{
  Semi-joins are implemented using \code{WHERE EXISTS}, and
  anti-joins with \code{WHERE NOT EXISTS}. Support for
  semi-joins is somewhat partial: you can only create semi
  joins where the \code{x} and \code{y} columns are
  compared with \code{=} not with more general operators.
}
\examples{
people <- tbl(src_lahman(), "Master")

# All people in half of fame
hof <- tbl(src_lahman(), "HallOfFame")
semi_join(people, hof)

# All people not in the hall of fame
semi_join(people, hof, anti = TRUE)

# Find all managers
manager <- tbl(src_lahman(), "Managers")
semi_join(people, manager)

# Find all managers in hall of fame
famous_manager <- semi_join(semi_join(people, manager), hof)
famous_manager
explain_tbl(famous_manager)
}

