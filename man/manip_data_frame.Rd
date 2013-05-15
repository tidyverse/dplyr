\name{manip_data_frame}
\alias{arrange.data.frame}
\alias{arrange.grouped_df}
\alias{filter.data.frame}
\alias{manip_data_frame}
\alias{mutate.data.frame}
\alias{mutate.grouped_df}
\alias{select.data.frame}
\alias{select.grouped_df}
\alias{summarise.data.frame}
\alias{summarise.grouped_df}
\title{Data manipulation for data frames.}
\usage{
  \method{filter}{data.frame} (.data, ...)

  \method{summarise}{data.frame} (.data, ...)

  \method{mutate}{data.frame} (.data, ...)

  \method{arrange}{data.frame} (.data, ...)

  \method{select}{data.frame} (.data, ...)

  \method{summarise}{grouped_df} (.data, ...)

  \method{mutate}{grouped_df} (.data, ...)

  \method{arrange}{grouped_df} (.data, ...)

  \method{select}{grouped_df} (.data, ...)
}
\description{
  Data manipulation for data frames.
}
\examples{
data("baseball", package = "plyr")
filter(baseball, year > 2005, g > 130)
head(select(baseball, id:team))
summarise(baseball, g = mean(g), n = count())
head(mutate(baseball, rbi = 1.0 * r / ab))
head(arrange(baseball, id, desc(year)))
}

