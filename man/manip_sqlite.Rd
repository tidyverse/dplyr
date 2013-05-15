\name{manip_sqlite}
\alias{arrange.source_sqlite}
\alias{filter.source_sqlite}
\alias{manip_sqlite}
\alias{mutate.source_sqlite}
\alias{select.source_sqlite}
\alias{summarise.source_sqlite}
\title{Data manipulation for SQL data sources.}
\usage{
  \method{filter}{source_sqlite} (.data, ...)

  \method{arrange}{source_sqlite} (.data, ...)

  \method{select}{source_sqlite} (.data, ...)

  \method{summarise}{source_sqlite} (.data, ...,
    .n = 1e+05)

  \method{mutate}{source_sqlite} (.data, ..., .n = 1e+05)
}
\arguments{
  \item{.data}{an SQLite data base}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{.n}{maximum number of columns to return. Set to
  \code{-1} to return all.}
}
\description{
  Arrange, filter and select are lazy: they modify the
  object representing the table, and do not recompute
  unless needed.  Summarise and mutate are eager: they will
  always return a source_df.
}
\examples{
baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")

# filter, select and arrange lazily modify the specification of the table
# they don't execute queries unless you print them
filter(baseball_s, year > 2005, g > 130)
select(baseball_s, id:team)
arrange(baseball_s, id, desc(year))

# summarise and mutate always return data frame sources
summarise(baseball_s, g = mean(g), n = count())
mutate(baseball_s, rbi = 1.0 * r / ab)
}

