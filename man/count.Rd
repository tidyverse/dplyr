\name{count}
\alias{count}
\title{Count.}
\usage{
count(x, ..., wt)
}
\arguments{
  \item{a}{\code{\link{tbl}} to count}

  \item{...}{variables to group by}

  \item{wt}{if not specified, will count the number of
  rows. If specified, will perform a "weighted" count but
  summing over the specified variable.}
}
\description{
Count is a convenient wrapper for summarise that either
call \code{\link{n}} or \code{\link{sum}(n)} depending on
whether you're counting for the first time, or recounting.
}
\examples{
if (require("Lahman")) {
  batting_tbl <- tbl_cpp(Batting)
  count(batting_tbl, yearID)

  plays_by_year <- count(batting_tbl, playerID, stint)
  count(plays_by_year, playerID)
  count(plays_by_year, stint)
}
}

