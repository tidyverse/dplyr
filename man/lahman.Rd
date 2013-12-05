\name{lahman}
\alias{has_lahman}
\alias{lahman}
\alias{lahman_bigquery}
\alias{lahman_df}
\alias{lahman_dt}
\alias{lahman_mysql}
\alias{lahman_postgres}
\alias{lahman_sqlite}
\alias{lahman_srcs}
\title{Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.}
\usage{
lahman_sqlite(path = NULL)

lahman_postgres(...)

lahman_mysql(...)

lahman_df()

lahman_dt()

lahman_bigquery(...)

has_lahman(type, ...)

lahman_srcs(..., quiet = NULL)
}
\arguments{
  \item{path}{location to look for and cache SQLite
  database. If \code{NULL}, the default, will first try
  storing in the installed package directory, and if that
  isn't writeable, a temporary directory.}

  \item{...}{Arguments passed to \code{src} on first load.
  For mysql and postgresql, the defaults assume you have a
  local server with \code{lahman} database already created.
  For bigquery, it assumes you have read/write access to a
  project called \code{Sys.getenv("BIGQUERY_PROJECT")}

  For \code{lahman_srcs}, character vector of names giving
  srcs to generate.}

  \item{quiet}{if \code{TRUE}, suppress messages about
  databases failing to connect.}

  \item{type}{src type.}
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
if (require("RSQLite") && has_lahman("sqlite")) {
  lahman_sqlite()
  batting <- tbl(lahman_sqlite(), "Batting")
  batting
}

# Connect to a local postgres database with lahman database, if available
if (require("RPostgreSQL") && has_lahman("postgres")) {
  lahman_postgres()
  batting <- tbl(lahman_postgres(), "Batting")
}
}

