\name{source_dt}
\alias{source_dt}
\title{Create a data table source.}
\usage{
  source_dt(data, name = NULL)
}
\arguments{
  \item{data}{a data table}

  \item{name}{the name of the data frame: used to help you
  remember where it came from. If not supplied, taken from
  the deparsed expression passed to the \code{data}
  argument.}
}
\description{
  A data table source wraps a local data table.
}
\examples{
ds <- source_dt(mtcars)
ds
as.data.table(ds)
as.source(mtcars)
}

