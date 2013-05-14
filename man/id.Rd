\name{id}
\alias{id}
\title{Compute a unique numeric id for each unique row in a data frame.}
\usage{
  id(.variables, drop = FALSE)
}
\arguments{
  \item{.variables}{list of variables}

  \item{drop}{drop unusued factor levels?}
}
\value{
  a numeric vector with attribute n, giving total number of
  possibilities
}
\description{
  Properties: \itemize{ \item \code{order(id)} is
  equivalent to \code{do.call(order, df)} \item rows
  containing the same data have the same value \item if
  \code{drop = FALSE} then room for all possibilites }
}
\seealso{
  \code{\link{id_var}}
}
\keyword{internal}

