\name{partial_eval}
\alias{partial_eval}
\title{Partially evaluate an expression.}
\usage{
  partial_eval(source, call, env = parent.frame())
}
\arguments{
  \item{source}{a data source object}

  \item{call}{an unevaluated expression, as produced by
  \code{\link{quote}}}

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
partial_eval(bdf, quote(year > 1980))

ids <- c("ansonca01", "forceda01", "mathebo01")
partial_eval(bdf, quote(id \%in\% ids))

# You can use local to disambiguate between local and remote
# variables: otherwise remote is always preferred
year <- 1980
partial_eval(bdf, quote(year > year))
partial_eval(bdf, quote(year > local(year)))

# Local is also needed if you want to call a local function
f <- function(x) x + 1
partial_eval(bdf, quote(year > f(1980)))
partial_eval(bdf, quote(year > local(f(1980))))
}
\keyword{internal}

