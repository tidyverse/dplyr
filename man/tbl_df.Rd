\name{tbl_df}
\alias{tbl_df}
\title{Create a data frame tble.}
\usage{
tbl_df(data)
}
\arguments{
  \item{data}{a data frame}
}
\description{
A data frame tbl wraps a local data frame. The main
advantage to using a \code{tbl_df} over a regular data
frame is the printing: tbl objects only print a few rows
and all the columns that fit on one screen, providing
describing the rest of it as text.
}
\examples{
ds <- tbl_df(mtcars)
ds
as.data.frame(ds)
as.tbl(mtcars)
}

