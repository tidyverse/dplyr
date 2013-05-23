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

  \item{where}{if not \code{NULL}, a character vector of
  conditions, which will be combined with \code{AND}}

  \item{group_by}{if not \code{NULL}, a character vector of
  named SQL expression used to group the data}

  \item{having}{}

  \item{order_by}{}

  \item{limit}{}

  \item{offset}{}
}
\description{
  Goal is to make valid sql given inputs - this knows
  nothing about sources.
}
\examples{
select_query("*", "mytable")
}
\keyword{internal}

