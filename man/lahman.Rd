\name{lahman}
\alias{has_lahman}
\alias{lahman}
\alias{lahman_bigquery}
\alias{lahman_mysql}
\alias{lahman_postgres}
\alias{lahman_sqlite}
\title{Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.}
\usage{
  lahman_sqlite(path = NULL)

  lahman_postgres(dbname = "lahman", ...)

  lahman_mysql(dbname = "lahman", ...)

  lahman_bigquery(project = "hadbilling", ...,
    quiet = FALSE)

  has_lahman(src)
}
\arguments{
  \item{path}{location to look for and cache SQLite
  database. If \code{NULL}, the default, will first try
  storing in the installed package directory, and if that
  isn't writeable, a temporary directory.}

  \item{dbname,...}{Arguments passed to \code{src} on first
  load. For mysql and postgresql, the defaults assume you
  have a local server with \code{lahman} database already
  created.}
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
lahman_sqlite()
batting <- tbl(lahman_sqlite(), "Batting")
batting

# Connect to a local postgres database with lahman database, if available
if (has_lahman("postgres")) {
  lahman_postgres()
  batting <- tbl(lahman_postgres(), "Batting")
}
}

