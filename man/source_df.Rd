\name{source_df}
\alias{source_df}
\title{Create a data frame source.}
\usage{
  source_df(data, name = NULL)
}
\arguments{
  \item{data}{a data frame}

  \item{name}{the name of the data frame: used to help you
  remember where it came from. If not supplied, taken from
  the deparsed expression passed to the \code{data}
  argument.}
}
\description{
  A data frame source wraps a local data frame. The main
  advantage to using a \code{source_df} over a regular data
  frame is the printing: source objects only print a few
  rows and all the columns that fit on one screen,
  providing describing the rest of it as text.
}
\examples{
ds <- source_df(mtcars)
ds
as.data.frame(ds)
as.source(mtcars)
}

