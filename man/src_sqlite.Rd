\name{src_sqlite}
\alias{src_sqlite}
\title{Create a SQLite3 data source}
\usage{
  src_sqlite(path, create = FALSE)
}
\arguments{
  \item{path}{Path to SQLite database}

  \item{create}{if \code{FALSE}, \code{path} must already
  exist. If \code{TRUE}, will create a new SQlite3 database
  at \code{path}.}
}
\description{
  This data structure is basically a pointer to an SQLite3
  database.
}
\examples{
\dontrun{
my_db <- src_sqlite(db_path)
src_tbls(my_db)
}

# A "built"-in dataset
lahman_sqlite()

# You can create a new sqlite database at any location if you set
# create = TRUE
new_db <- src_sqlite(tempfile(), TRUE)
src_tbls(new_db)
}

