\name{source_sql}
\alias{source_sql}
\title{Create an SQL data source (abstract)}
\usage{
  source_sql(subclass, ..., select = NULL, filter = NULL,
    arrange = NULL)
}
\arguments{
  \item{subclass}{name of subclass}

  \item{...}{other fields needed by the subclass}

  \item{select,filter,arrange}{default sql components.}
}
\description{
  This method shouldn't be called be users - it should only
  be used by backend implementors who are creating backends
  that extend the basic sql behaviour.
}
\keyword{internal}

