\name{compound.tbl_sqlite}
\alias{compound.tbl_sqlite}
\title{Create a compound sqlite tbl}
\usage{
  \method{compound}{tbl_sqlite} (x, y)
}
\description{
  Create a compound sqlite tbl
}
\examples{
l <- lahman()
batting <- select(tbl(l, "Batting"), playerID:lgID, G_batting)
pitching <- select(filter(tbl(l, "Fielding"), POS == "P"), playerID:lgID, GS)

both <- compound(batting, pitching)
hou <- filter(plays, teamID == "HOU")
head(hou)
}

