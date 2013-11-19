\name{lead-lag}
\alias{lag}
\alias{lead}
\alias{lead-lag}
\title{Lead and lag.}
\usage{
lead(x, n = 1L, default = NA)

lag(x, n = 1L, default = NA)
}
\arguments{
  \item{x}{a vector of values}

  \item{n}{a postive integer of length 1, giving the number
  of positions to lead or lag by}

  \item{default}{value used for non-existant rows. Defaults
  to \code{NA}.}
}
\description{
Lead and lag are useful for comparing values offset by a
constant (e.g. the previous or next value)
}
\examples{
lead(1:10, 1)
lead(1:10, 2)

lag(1:10, 1)
lead(1:10, 1)

x <- runif(5)
cbind(ahead = lead(x), x, behind = lag(x))
}

