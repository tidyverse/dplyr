\name{lahman_db}
\alias{lahman}
\alias{lahman_db}
\title{Create a database version of the Lahman baseball database.}
\usage{
  lahman_db(src, index = TRUE, quiet = FALSE)

  lahman(path = NULL)
}
\arguments{
  \item{src}{a data source with write access}
}
\description{
  \code{lahman_db} creates a copy of the Lahman baseball
  database in the database src of your choice.
  \code{lahman} creates a cached version of the Lahman
  database in a standard location for use in examples.
}
\details{
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

