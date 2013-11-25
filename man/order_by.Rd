\name{order_by}
\alias{order_by}
\title{A helper function for ordering window function output.}
\usage{
order_by(order_by, call)
}
\arguments{
  \item{order_by}{a vector to order_by}

  \item{call}{a function call to a window function, where
  the first argument is the vector being operated on}
}
\description{
This is a useful function to control the order of window
functions in R that don't have a specific ordering
parameter. When translated to SQL it will modify the order
clause of the OVER function.
}
\details{
This function works by changing the \code{call} to instead
call \code{\link{with_order}} with the appropriate
arguments.
}
\examples{
df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]

wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)

right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)
}

