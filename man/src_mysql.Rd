\name{src_mysql}
\alias{src_mysql}
\title{Connect to mysql/mariadb.}
\usage{
  src_mysql(dbname, host = NULL, port = 0L, user = "root",
    password = "", ...)
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
  existing mysql database. If you are running a local
  mysqlql database, leave all parameters set as their
  defaults to connect. If you're connecting to a remote
  database, ask your database administrator for the values
  of these variables.
}
\examples{
\dontrun{
my_db <- src_mysql()
src_tbls(my_db)

l <- src_mysql("lahman")
batting <- tbl(l, "Batting")
}
}

