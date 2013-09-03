\name{write_table}
\alias{write_table}
\title{Create a new \code{tbl_sqlite} from a data frame}
\usage{
  write_table(src, table, value, types = NULL,
    temporary = FALSE)
}
\arguments{
  \item{src}{SQLite data source, to which you have write
  access}

  \item{table}{name of table to create.}

  \item{value}{a local data frame}

  \item{temporary}{if \code{TRUE}, will create a temporary
  table that is local to this connection and will be
  automatically deleted when the connection expires}
}
\value{
  a \code{\link{tbl_sqlite}} object
}
\description{
  This uploads a local data frame into a remote data
  source, creating the table definition as needed.
}

