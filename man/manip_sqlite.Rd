\name{manip_sqlite}
\alias{arrange.source_sqlite}
\alias{filter.source_sqlite}
\alias{manip_sqlite}
\alias{mutate.source_sqlite}
\alias{select.source_sqlite}
\alias{summarise.source_sqlite}
\title{Data manipulation for SQL data sources.}
\usage{
  \method{filter}{source_sqlite} (.data, ..., .n = 1e+05)

  \method{summarise}{source_sqlite} (.data, ...,
    .n = 1e+05)

  \method{mutate}{source_sqlite} (.data, ..., .n = 1e+05)

  \method{arrange}{source_sqlite} (.data, ..., .n = 1e+05)

  \method{select}{source_sqlite} (.data, ..., .n = 1e+05)
}
\description{
  Data manipulation for SQL data sources.
}
\examples{
baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
filter(baseball_s, year > 2005, g > 130)
head(select(baseball_s, id:team))
summarise(baseball_s, g = mean(g), n = count())
head(mutate(baseball_s, rbi = 1.0 * r / ab))
head(arrange(baseball_s, id, desc(year)))
}

