\name{compound}
\alias{compound}
\title{Create a compound select.}
\usage{
  compound(..., type)
}
\arguments{
  \item{...}{\code{tbl}s to combine}

  \item{type}{type of combination.}
}
\description{
  Combine together the results of multiple SQL queries,
}
\examples{
l <- lahman()
batting <- select(tbl(l, "Batting"), playerID:lgID, G_batting)
pitching <- select(filter(tbl(l, "Fielding"), POS == "P"), playerID:lgID, GS)
outfield <- select(filter(tbl(l, "Fielding"), POS == "OF"), playerID:lgID, GS)

plays <- compound(batting, pitching, outfield)
hou <- filter(plays, teamID == "HOU")
}

