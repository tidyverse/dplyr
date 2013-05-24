\name{dplyr-formatting}
\alias{dim_desc}
\alias{dplyr-formatting}
\alias{trunc_mat}
\title{Tools for describing matrices}
\usage{
  dim_desc(x)

  trunc_mat(x, n = 10L)
}
\arguments{
  \item{x}{a matrix to describe}

  \item{n}{number of rows to show}
}
\description{
  Tools for describing matrices
}
\examples{
dim_desc(mtcars)
trunc_mat(mtcars)
}
\keyword{internal}

