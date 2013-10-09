\name{chain}
\alias{\%.\%}
\alias{chain}
\alias{chain_q}
\title{Chain together multiple operations.}
\usage{
  chain(..., env = parent.frame())

  chain_q(calls, env = parent.frame())

  %.%(x, y)
}
\arguments{
  \item{x,y}{A dataset and function to apply to it}

  \item{...,calls}{A sequence of data transformations,
  starting with a dataset.  The first argument of each call
  should be omitted - the value of the previous step will
  be substituted in automatically.  Use \code{chain} and
  \code{...} when working interactive; use \code{chain_q}
  and \code{calls} when calling from another function.}

  \item{env}{Environment in which to evaluation
  expressions. In ordinary operation you should not need to
  set this parameter.}
}
\description{
  The downside of the functional nature of dplyr is that
  when you combine multiple data manipulation operations,
  you have to read from the inside out and the arguments
  may be very distant to the function call. These functions
  providing an alternative way of calling dplyr (and other
  data manipulation) functions that you read can from left
  to right.
}
\details{
  The functions work via simple substitution so that
  \code{chain(x, f(y))} or \code{x %.% f(y)} is translated
  into \code{f(x, y)}.
}
\examples{
# If you're performing many operations you can either do step by step
a1 <- group_by(hflights, Year, Month, DayofMonth)
a2 <- select(a1, Year:DayofMonth, ArrDelay, DepDelay)
a3 <- summarise(a2,
  arr = mean(ArrDelay, na.rm = TRUE),
  dep = mean(DepDelay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

# If you don't want to save the intermediate results, you need to
# wrap the functions:
filter(
  summarise(
    select(
      group_by(hflights, Year, Month, DayofMonth),
      Year:DayofMonth, ArrDelay, DepDelay
    ),
    arr = mean(ArrDelay, na.rm = TRUE),
    dep = mean(DepDelay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

# This is difficult to read because the order of the operations is from
# inside to out, and the arguments are a long way away from the function.
# Alternatively you can use chain or \%.\% to sequence the operations
# linearly:

hflights \%.\%
  group_by(Year, Month, DayofMonth) \%.\%
  select(Year:DayofMonth, ArrDelay, DepDelay) \%.\%
  summarise(
    arr = mean(ArrDelay, na.rm = TRUE),
    dep = mean(DepDelay, na.rm = TRUE)
  ) \%.\%
  filter(a3, arr > 30 | dep > 30)

chain(
  hflights,
  group_by(Year, Month, DayofMonth),
  select(Year:DayofMonth, ArrDelay, DepDelay),
  summarise(
    arr = mean(ArrDelay, na.rm = TRUE),
    dep = mean(DepDelay, na.rm = TRUE)
  ),
  filter(a3, arr > 30 | dep > 30)
)
}

