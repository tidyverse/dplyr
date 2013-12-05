\name{count_distinct}
\alias{count_distinct}
\title{Efficiently count the number of unique values in a vector.}
\usage{
count_distinct(x)
}
\arguments{
  \item{x}{a vector of values}
}
\description{
This is a faster and more concise equivalent of
\code{length(unique(x))}
}
\examples{
x <- sample(1:10, 1e5, rep = TRUE)
length(unique(x))
count_distinct(x)
}

