\name{tbl}
\alias{as.tbl}
\alias{is.tbl}
\alias{tbl}
\title{Create a "tbl" object}
\usage{
  tbl(subclass, ...)

  is.tbl(x)

  as.tbl(x, ...)
}
\arguments{
  \item{subclass}{name of subclass. "tbl" is an abstract,
  so you must supply this value.}

  \item{object}{to test/coerce.}

  \item{...}{For \code{tbl}, other fields used by class.
  For \code{as.tbl}, other arguments passed to methods.}
}
\description{
  \code{tbl} is the standard constructor for tbls.
  \code{as.tbl} coerces, and \code{is.tbl} tests.
}
\examples{
as.tbl(mtcars)
}
\keyword{internal}

