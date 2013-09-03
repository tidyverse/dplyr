\name{src_sqlite}
\alias{src_sqlite}
\title{Create a SQLite data source}
\usage{
  src_sqlite(path)
}
\arguments{
  \item{path}{Path to SQLite database}
}
\description{
  This data structure is basically a pointer to an SQLite
  database.
}
\examples{
db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball <- src_sqlite(db_path)
src_tbls(baseball)
}

