\name{count}
\alias{count}
\title{Count observations by group.}
\usage{
count(x, wt)
}
\arguments{
  \item{a}{\code{\link{tbl}} to count}

  \item{wt}{if not specified, will count the number of
  rows. If specified, will perform a "weighted" count but
  summing over the specified variable.}
}
\description{
\code{count} is a convenient wrapper for summarise that
either call \code{\link{n}} or \code{\link{sum}(n)}
depending on whether you're counting for the first time, or
recounting.
}
\examples{
if (require("Lahman")) {
  batting_tbl <- tbl_cpp(Batting)
  count(group_by(batting_tbl, yearID))

  plays_by_year <- count(group_by(batting_tbl, playerID, stint))
  count(plays_by_year)
  count(count(plays_by_year))
  count(group_by(plays_by_year, stint))

  # This looks a little nicer if you use the infix \%.\% operator
  batting_tbl \%.\% group_by(playerID) \%.\% count()
}
}

