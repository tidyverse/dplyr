\name{lapply_cluster}
\alias{lapply_cluster}
\title{Cluster lapply.}
\usage{
lapply_cluster(.x, .f, ..., .cl = init_cluster())
}
\arguments{
  \item{.x}{list of elements to apply over}

  \item{.f}{function to apply}

  \item{...}{additional arguments to \code{.f}}

  \item{.cl}{cluster to use}
}
\description{
A low-level implementation of \code{\link{lapply}} which
splits tasks tasks across nodes in a cluster.
}
\examples{
lapply_cluster(1:10, sqrt)
}
\keyword{internal}

