\name{manip_grouped_sqlite}
\alias{arrange.grouped_sqlite}
\alias{filter.grouped_sqlite}
\alias{manip_grouped_sqlite}
\alias{mutate.grouped_sqlite}
\alias{select.grouped_sqlite}
\alias{summarise.grouped_sqlite}
\title{Data manipulation for grouped SQL data sources.}
\usage{
  \method{filter}{grouped_sqlite} (.data, ...)

  \method{summarise}{grouped_sqlite} (.data, ...,
    .n = 1e+05)

  \method{mutate}{grouped_sqlite} (.data, ..., .n = 1e+05)

  \method{arrange}{grouped_sqlite} (.data, ...)

  \method{select}{grouped_sqlite} (.data, ...)
}
\arguments{
  \item{.data}{an SQLite data base}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{.n}{maximum number of columns to return. Set to
  \code{-1} to return all.}
}
\description{
  Data manipulation for grouped SQL data sources.
}
\examples{
db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_s <- source_sqlite(db_path, "baseball")
players <- group_by(baseball_s, id)

# Due to the lack of windowing functions in SQLite, only summarising
# is actually useful
summarise(players, g = mean(g))
summarise(players, g = mean(g), best_ab = max(ab))

per_year <- group_by(baseball_s, id, year)
stints <- summarise(per_year, stints = max(stint))
filter(stints, stints > 3)

# All other operations will ignore grouping, although they will preserve it
# in the object returned to R.
filter(players, g > 100)
mutate(players, rbi = 1 * r / ab)
arrange(players, id, desc(year))
select(players, id:team)

# NB: If you use an aggregation function, you will get one row:
mutate(players, cyear = year - min(year) + 1)
}

