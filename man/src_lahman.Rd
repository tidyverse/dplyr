\name{src_lahman}
\alias{src_lahman}
\title{Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.}
\usage{
  src_lahman(path = NULL)
}
\arguments{
  \item{path}{location to look for and cache database. If
  \code{NULL}, the default, will first try storing in the
  installed package directory, and if that isn't writeable,
  a temporary directory.}
}
\description{
  This creates an interesting database using data from the
  Lahman baseball data source, provided by Sean Lahman at
  \url{http://www.seanlahman.com/baseball-archive/statistics/},
  and made easily available in R through the \pkg{Lahman}
  package by Michael Friendly, Dennis Murphy and Martin
  Monkman. See the documentation for that package for
  documentation of the inidividual tables.
}
\examples{
src_lahman()
batting <- tbl(src_lahman(), "Batting")
batting
}

