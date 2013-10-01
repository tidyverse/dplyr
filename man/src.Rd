\name{src}
\alias{is.src}
\alias{src}
\title{Create a "src" object}
\usage{
  src(subclass, ...)

  is.src(x)
}
\arguments{
  \item{subclass}{name of subclass. "src" is an abstract
  base class, so you must supply this value. \code{src_} is
  automatically prepended to the class name}

  \item{...}{fields used by object}

  \item{x}{object to test for "src"-ness.}
}
\description{
  \code{src} is the standard constructor for srcs and
  \code{is.src} tests.
}
\keyword{internal}

