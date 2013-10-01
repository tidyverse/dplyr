\name{src_postgres}
\alias{src_postgres}
\title{Create a postgres data source}
\usage{
  src_postgres(dbname = "", host = "", user = "",
    password = "", port = "", options = "", create = FALSE)
}
\arguments{
  \item{path}{Path to postgres database}

  \item{create}{if \code{FALSE}, \code{path} must already
  exist. If \code{TRUE}, will create a new postgres3
  database at \code{path}.}
}
\description{
  This data structure is basically a pointer to an postgres
  database.
}
\examples{
\dontrun{
my_db <- src_postgres()
src_tbls(my_db)
}

src_postgres("lahman")
}

