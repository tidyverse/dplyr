\name{source_dt}
\alias{source_dt}
\title{Create a data table source.}
\usage{
  source_dt(data)
}
\arguments{
  \item{data}{a data table}
}
\description{
  A data table source wraps a local data table.
}
\examples{
if (require("data.table")) {
ds <- source_dt(mtcars)
ds
as.data.table(ds)
as.source(mtcars)
}
}

