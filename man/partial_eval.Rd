\name{partial_eval}
\alias{partial_eval}
\title{Partially evaluate an expression.}
\usage{
  partial_eval(call, source = NULL, env = parent.frame())
}
\arguments{
  \item{call}{an unevaluated expression, as produced by
  \code{\link{quote}}}

  \item{source}{a tbl object}

  \item{env}{environment in which to search for local
  values}
}
\description{
  This function partially evaluates an expression, using
  information from the tbl to determine whether names refer
  to local expressions or remote variables. This simplifies
  SQL translation because expressions don't need to carry
  around their environment - all revelant information is
  incorporated into the expression.
}
\section{Symbol substitution}{
  \code{partial_eval} needs to guess if you're referring to
  a variable on the server (remote), or in the current
  environment (local). It's not possible to do this 100%
  perfectly. \code{partial_eval} uses the following
  heuristic:

  \itemize{ \item If the source variables are known, and
  the symbol matches a source variable, then remote.  \item
  If the symbol is defined locally, local.  \item
  Otherwise, remote. }
}
\examples{
data("baseball", package = "plyr")
bdf <- tbl_df(baseball)
partial_eval(quote(year > 1980), bdf)

ids <- c("ansonca01", "forceda01", "mathebo01")
partial_eval(quote(id \%in\% ids), bdf)

# You can use local to disambiguate between local and remote
# variables: otherwise remote is always preferred
year <- 1980
partial_eval(quote(year > year), bdf)
partial_eval(quote(year > local(year)), bdf)

# Functions are always assumed to be remote. Use local to force evaluation
# in R.
f <- function(x) x + 1
partial_eval(quote(year > f(1980)), bdf)
partial_eval(quote(year > local(f(1980))), bdf)

# For testing you can also use it with the source omitted
partial_eval(quote(1 + 2 * 3))
x <- 1
partial_eval(quote(x ^ y))
}
\keyword{internal}

