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
select(batting, yearID:AB)

# Compute career year, rank of at bats, and cumulative at bats
players <- group_by(batting, playerID)
progress <- mutate(players, cyear = yearID - min(yearID) + 1,
 rank(desc(AB)), cumsum(AB, yearID))

# Progressively summarise data
per_year <- group_by(batting, playerID, yearID)
stints <- summarise(per_year, stints = max(stint))
filter(stints, stints > 3)
summarise(stints, max(stints))

# Subset grouped data
players <- group_by(batting, playerID)
best_year <- subset(players, AB == max(AB) || G == max(G))
}

