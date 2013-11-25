\name{ranking}
\alias{cume_dist}
\alias{dense_rank}
\alias{min_rank}
\alias{ntile}
\alias{percent_rank}
\alias{ranking}
\alias{row_number}
\title{Windowed rank functions}
\usage{
row_number(x)

ntile(x, n)

min_rank(x)

dense_rank(x)

percent_rank(x)

cume_dist(x)
}
\arguments{
  \item{x}{a vector of values to rank}

  \item{n}{number of groups to split up into.}
}
\description{
Windowed rank functions
}
\examples{
x <- c(5, 1, 3, 2, 2)
row_number(x)
min_rank(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)

ntile(x, 2)
ntile(runif(100), 10)
}

