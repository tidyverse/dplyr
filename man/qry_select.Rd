\name{qry_select}
\alias{qry_select}
\title{Generate a basic SQL \code{SELECT} query.}
\usage{
  qry_select(x, select, from, where = NULL,
    group_by = NULL, having = NULL, order_by = NULL,
    limit = NULL, offset = NULL, ...)
}
\arguments{
  \item{select}{a character vector of fields to select.
  Names are used to create \code{AS} aliases.}

  \item{from}{a string giving the table name}

  \item{where,group_by,having,order_by,limit,offset}{Select
  query components. All inputs are language objects}
}
\value{
  a \code{\link{query}} object
}
\description{
  Generate a basic SQL \code{SELECT} query.
}
\examples{
qry_select(lahman_sqlite(), list(star()), ident("mytable"))
qry_select(lahman_sqlite(), list(star()), ident("mytable"), list(quote(1 == 0)))
}
\keyword{internal}

