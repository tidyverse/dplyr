\name{src_postgres}
\alias{src_postgres}
\title{Create a postgres data source}
\usage{
  src_postgres(dbname = "", host = "", port = "",
    user = "", password = "", options = "", create = FALSE)
}
\arguments{
  \item{dbname}{Database}

  \item{host,port}{Host name and port number of database}

  \item{user,password}{User name and password (if needed)}

  \item{options}{other additional options passed to command
  line client}

  \item{create}{if \code{FALSE}, \code{dbname} must already
  exist. If \code{TRUE}, will create a new postgres - to do
  this you must either be a super user or have the CREATE
  DB permssion.}
}
\description{
  This data structure represents a connection to an
  existing postgres database. If you are running a local
  postgresql database, leave all parameters set as their
  defaults to connect. If you're connecting to a remote
  database, ask your database administrator for the values
  of these variables.
}
\examples{
\dontrun{
my_db <- src_postgres()
src_tbls(my_db)

src_postgres("lahman")
}
}

