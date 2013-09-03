\name{lahman_db}
\alias{lahman_db}
\title{Create a database version of the Lahman baseball database.}
\usage{
  lahman_db(src)
}
\arguments{
  \item{src}{a data source with write access}
}
\description{
  This creates an interesting database using data from the
  Lahman baseball data source, provided by Sean Lahman at
  \url{http://www.seanlahman.com/baseball-archive/statistics/},
  and made easily available in R through the \pkg{Lahman}
  package by Michael Friendly, Dennis Murphy and Martin
  Monkman.
}
\examples{
\dontrun{
db <- src_sqlite("~/desktop/lahman.sqlite", create = TRUE)
lahman_db(db)
}
}

