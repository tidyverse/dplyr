\name{nth_value}
\alias{first_value}
\alias{last_value}
\alias{nth_value}
\title{Extract the first, last or nth value from a vector.}
\usage{
nth_value(x, n, order_by = NULL)

first_value(x, order_by = NULL)

last_value(x, order_by = NULL)
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

last_value(x)
last_value(x, y)
}

