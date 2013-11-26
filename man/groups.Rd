\name{groups}
\alias{groups}
\alias{groups<-}
\title{Get/set the grouping variables for tbl.}
\usage{
groups(x)

groups(x) <- value
}
\arguments{
  \item{x}{data \code{\link{tbl}}}

  \item{value}{a list of symbols}
}
\description{
These functions do not perform non-standard evaluation, and
so are useful when programming against \code{tbl} objects.
}
\examples{
grouped <- group_by(mtcars, cyl)
groups(grouped)
groups(grouped) <- list(quote(vs))
groups(grouped)
}
\seealso{
\code{\link{group_by}} for a version that does non-standard
evaluation to save typing
}

