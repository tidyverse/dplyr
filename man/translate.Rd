\name{translate}
\alias{translate}
\title{Translate an R expression to the language of another data store}
\usage{
  translate(source, call, env = parent.frame())
}
\arguments{
  \item{source}{a data source object}

  \item{call}{an unevaluated expression, as produced by
  \code{\link{quote}}}

  \item{env}{environment in which to search for local
  values}
}
\description{
  Translate an R expression to the language of another data
  store
}
\examples{
baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
teams <- c("RC1", "CL1")
expr <- quote(year < 1890 & g > 20 & team \%in\% teams)
partial_eval(baseball_s, expr)
translate(baseball_s, expr)
}
\keyword{internal}

