\name{manip_sqlite}
\alias{arrange.tbl_sqlite}
\alias{filter.tbl_sqlite}
\alias{manip_sqlite}
\alias{mutate.tbl_sqlite}
\alias{select.tbl_sqlite}
\alias{summarise.tbl_sqlite}
\title{Data manipulation for SQL tbls.}
\usage{
  \method{filter}{tbl_sqlite} (.data, ...)

  \method{arrange}{tbl_sqlite} (.data, ...)

  \method{select}{tbl_sqlite} (.data, ...)

  \method{summarise}{tbl_sqlite} (.data, ..., .n = 1e+05)

  \method{mutate}{tbl_sqlite} (.data, ..., .n = 1e+05)
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
  always return a tbl_df.
}
\examples{
db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_s <- tbl_sqlite(db_path, "baseball")

# filter, select and arrange lazily modify the specification of the table
# they don't execute queries unless you print them
filter(baseball_s, year > 2005, g > 130)
select(baseball_s, id:team)
arrange(baseball_s, id, desc(year))

# summarise and mutate always return data frame tbls
summarise(baseball_s, g = mean(g), n = count())
mutate(baseball_s, rbi = 1.0 * r / ab)

# Grouped summaries -----------------------------------
players <- group_by(baseball_s, id)

# Due to the lack of windowing functions in SQLite, only summarising
# is really useful with grouped values
summarise(players, g = mean(g))
summarise(players, g = mean(g), best_ab = max(ab))

per_year <- group_by(baseball_s, id, year)
stints <- summarise(per_year, stints = max(stint))
collect(filter(stints, stints > 3))

# All other operations will ignore grouping, although they will preserve it
# in the object returned to R.
filter(players, g > 100)
mutate(players, rbi = 1 * r / ab)
arrange(players, id, desc(year))
select(players, id:team)

# NB: If you use an aggregation function, you will get one row:
mutate(players, cyear = year - min(year) + 1)
}

