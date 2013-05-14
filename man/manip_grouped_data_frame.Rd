\name{manip_grouped_data_frame}
\alias{filter.grouped_data_frame}
\alias{manip_grouped_data_frame}
\title{Data manipulation for grouped data frames.}
\usage{
  \method{filter}{grouped_data_frame} (.data, ...)
}
\description{
  Data manipulation for grouped data frames.
}
\examples{
data("baseball", package = "plyr")
players <- group_by(baseball, id)
filter(players, g == max(g))
summarise(players, g = mean(g))
head(mutate(players, cyear = year - min(year) + 1))
head(arrange(players, year))
head(select(players, id:team))
}

