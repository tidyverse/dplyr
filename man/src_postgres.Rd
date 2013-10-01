\name{src_postgres}
\alias{src_postgres}
\title{Create a postgres data source}
\usage{
  src_postgres(dbname = "", host = "", port = "",
    user = "", password = "", options = "")
}
\arguments{
  \item{dbname}{Database}

  \item{host,port}{Host name and port number of database}

  \item{user,password}{User name and password (if needed)}

  \item{options}{other additional options passed to command
  line client}
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

l <- src_postgres("lahman")
batting <- tbl(l, "Batting")
}
}

