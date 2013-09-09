\name{do}
\alias{do}
\title{Apply a function to a tbl}
\usage{
  do(.data, .f, ...)
}
\arguments{
  \item{.data}{a tbl}

  \item{.f}{a function to apply. The first unnamed argument
  supplied to \code{.f} will be a data frame.}

  \item{...}{other arguments passed on to the function ()}
}
\description{
  This is a general purpose complement to the specialised
  manipulation functions \code{\link{filter}},
  \code{\link{select}}, \code{\link{mutate}},
  \code{\link{summarise}} and \code{\link{arrange}}.
}
\examples{
by_dest <- group_by(hflights, Dest)
do(by_dest, nrow)
# Inefficient version of
group_size(by_dest)

# You can use it to do any arbitrary computation, like fitting a linear
# model. Let's explore how carrier departure delays vary over the course
# of a year
hflights <- mutate(hflights, date = ISOdate(Year, Month, DayofMonth))
carriers <- group_by(hflights, UniqueCarrier)
group_size(carriers)

mods <- do(carriers, failwith(NULL, lm), formula = ArrDelay ~ date)
sapply(mods, coef)
}

