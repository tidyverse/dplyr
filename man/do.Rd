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
by_player <- group_by(baseball, id)
by_player <- mutate(by_player, cyear = year - min(year) + 1)
system.time(mods <- do(by_player, lm, formula = g ~ poly(cyear, 2)))

by_player <- group_by(as.data.table(baseball), id)
by_player <- mutate(by_player, cyear = year - min(year) + 1)
system.time(mods <- do(by_player, lm, formula = g ~ poly(cyear, 2)))
}

