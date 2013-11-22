\name{tally}
\alias{tally}
\title{Tally observations by group.}
\usage{
tally(x, wt)
}
\arguments{
  \item{x}{a \code{\link{tbl}} to tally}

  \item{wt}{if not specified, will tally the number of
  rows. If specified, will perform a "weighted" tally but
  summing over the specified variable.}
}
\description{
\code{tally} is a convenient wrapper for summarise that
either call \code{\link{n}} or \code{\link{sum}(n)}
depending on whether you're tallying for the first time, or
re-tallying.
}
\examples{
if (require("Lahman")) {
  batting_tbl <- tbl_cpp(Batting)
  tally(group_by(batting_tbl, yearID))

  plays_by_year <- tally(group_by(batting_tbl, playerID, stint))
  tally(plays_by_year)
  # FIXME: https://github.com/hadley/dplyr/issues/129
  # tally(tally(plays_by_year))
  tally(group_by(plays_by_year, stint))

  # This looks a little nicer if you use the infix \%.\% operator
  batting_tbl \%.\% group_by(playerID) \%.\% tally()
}
}

