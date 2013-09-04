\name{select_query}
\alias{select_query}
\title{Generate an SQL select query.}
\usage{
  select_query(select, from, where = NULL, group_by = NULL,
    having = NULL, order_by = NULL, limit = NULL,
    offset = NULL)
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
\description{
  Goal is to make valid sql select query inputs - this
  knows nothing about sources.
}
\examples{
select_query(sql("*"), ident("mytable"))
select_query(sql("*"), ident("mytable"), sql("1 = 0"))
}
\keyword{internal}

