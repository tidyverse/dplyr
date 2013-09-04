\name{cache}
\alias{cache}
\title{Cache a compound tbl.}
\usage{
  cache(x, name = random_table_name(), ...)
}
\arguments{
  \item{x}{a data tbl}

  \item{name}{name to use for temporary table. Defaults to
  a random name.}

  \item{...}{other arguments passed on to methods}
}
\description{
  This precomputes a compound tbl and saves it (if
  possible) in the remote src. It's like
  \code{as.data.frame} but instead of bringing all the data
  back locally, it saves it remotely. For sql datasources,
  it will create a temporary table.
}
\examples{
lah <- lahman()
batting <- select(filter(tbl(lah, "Batting"), stint == 1), playerID:H)
team <- select(tbl(lah, "Teams"), yearID, lgID, teamID, G, R:H)
both <- join(batting, team, type = "inner", by = c("yearID", "teamID", "lgID"))
hou <- filter(both, teamID == "HOU")
hou <- cache(hou)
}

