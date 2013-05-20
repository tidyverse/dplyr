\name{partial_eval}
\alias{partial_eval}
\title{Partially evaluate an expression.}
\usage{
  partial_eval(call, source, env = parent.frame())
}
\arguments{
  \item{call}{an unevaluated expression, as produced by
  \code{\link{quote}}}

  \item{source}{a data source object}

  \item{env}{environment in which to search for local
  values}
}
\description{
  This function partially evaluates an expression, using
  information from the data source to determine whether
  names refer to local expressions or remote variables.
}
\examples{
data("baseball", package = "plyr")
bdf <- source_df(baseball)
partial_eval(quote(year > 1980), bdf)

ids <- c("ansonca01", "forceda01", "mathebo01")
partial_eval(quote(id \%in\% ids), bdf)

# You can use local to disambiguate between local and remote
# variables: otherwise remote is always preferred
year <- 1980
partial_eval(quote(year > year), bdf)
partial_eval(quote(year > local(year)), bdf)

# Local is also needed if you want to call a local function
f <- function(x) x + 1
partial_eval(quote(year > f(1980)), bdf)
partial_eval(quote(year > local(f(1980))), bdf)
}
\keyword{internal}

