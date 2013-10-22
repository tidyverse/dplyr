\name{tbl_cpp}
\alias{tbl_cpp}
\title{Create a data frame tble.}
\usage{
  tbl_cpp(data)
}
\arguments{
  \item{data}{a data frame}
}
\description{
  Create a data frame tble.
}
\examples{
ds <- tbl_cpp(mtcars)
ds
as.data.frame(ds)
}

