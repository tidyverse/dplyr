\name{manip_postgres}
\alias{manip_postgres}
\alias{summarise.tbl_postgres}
\title{Data manipulation for postgre tbls.}
\usage{
  \method{summarise}{tbl_postgres} (.data, ...)
}
\description{
  This page documents the specific of data manipulation for
  \code{\link{tbl_postgresq}} objects. See
  \code{\link{manip}} for the documentation of generics
  functions, and \code{\link{manip_sql}} for general SQL
  documentation.
}
\examples{
batting <- tbl(src_postgres("lahman"), "Batting")
players <- group_by(batting, playerID)
mutate(players, cyear = yearID - min(yearID) + 1,
 rank(desc(AB)), cumsum(AB, yearID))

mutate(players, rank(AB), cumsum(AB, yearID))
}

