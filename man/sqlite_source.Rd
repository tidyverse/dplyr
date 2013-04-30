\name{sqlite_source}
\alias{sqlite_source}
\title{Create an sqlite data source.}
\usage{
  sqlite_source(path, table)
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
baseball_s <- sqlite_source(path, "baseball")
dim(baseball_s)
names(baseball_s)
head(baseball_s)
}

