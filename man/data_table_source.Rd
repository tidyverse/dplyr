\name{data_table_source}
\alias{data_table_source}
\title{Create a data table source.}
\usage{
  data_table_source(data, name = NULL)
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
ds <- data_table_source(mtcars)
ds
as.data.frame(ds)
as.source(mtcars)
}

