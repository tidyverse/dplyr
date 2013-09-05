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

  \item{from,where,group_by,having,order_by,limit,offset}{Select
  query components. All inputs are \code{\link{escape}}d,
  so make sure they have been wrapped appropriately with
  \code{\link{sql}} or \code{\link{ident}}.}
}
\value{
  a \code{\link{query}} object
}
\description{
  Generate a basic SQL \code{SELECT} query.
}
\examples{
select_query(l(), sql("*"), ident("mytable"))
select_query(l(), sql("*"), ident("mytable"), sql("1 = 0"))
}
\keyword{internal}

