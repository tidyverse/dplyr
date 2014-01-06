\name{nth}
\alias{first}
\alias{last}
\alias{nth}
\title{Extract the first, last or nth value from a vector.}
\usage{
nth(x, n, order_by = NULL)

first(x, order_by = NULL)

last(x)
}
\arguments{
  \item{x}{A vector}

  \item{n}{For \code{nth_value}, a single integer
  specifying the position.  If larger than x, an \code{NA}
  of the same type as x will be returned.}

  \item{order_by}{An optional vector used to determine the
  order}
}
\description{
These are straightforward wrappers around \code{\link{[}}.
The main advantage is that you can provide an optional
secondary vector that defines the ordering.
}
\examples{
x <- 1:10
y <- 10:1

last(x)
last(x, y)
}

