\name{source_sqlite}
\alias{source_sqlite}
\title{Create an sqlite data source.}
\usage{
  source_sqlite(path, table)
}
\arguments{
  \item{path}{path to sqlite database}

  \item{table}{name of table in database}
}
\description{
  Create an sqlite data source.
}
\examples{
path <- system.file("db/baseball.sqlite3", package = "dply")
path <- "inst/db/baseball.sqlite3"
baseball_s <- source_sqlite(path, "baseball")
dim(baseball_s)
names(baseball_s)
head(baseball_s)

players <- group(baseball_s, id)
summarise(players, g = mean(g), n = count())
}

