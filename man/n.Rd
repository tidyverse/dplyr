\name{n}
\alias{n}
\title{The number of observations in the current group.}
\usage{
n()
}
\description{
This function is implemented special for each data source
and can only be used from within \code{\link{summarise}}.
}
\examples{
if (require("hflights")) {
carriers <- group_by(hflights, UniqueCarrier)
summarise(carriers, n())
}
}

