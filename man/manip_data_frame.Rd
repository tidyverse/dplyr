\name{manip_data_frame}
\alias{arrange.data.frame}
\alias{filter.data.frame}
\alias{manip_data_frame}
\alias{mutate.data.frame}
\alias{select.data.frame}
\alias{summarise.data.frame}
\title{Data manipulation for data frames.}
\usage{
  \method{filter}{data.frame} (.data, ...)

  \method{summarise}{data.frame} (.data, ...)

  \method{mutate}{data.frame} (.data, ...)

  \method{arrange}{data.frame} (.data, ...)

  \method{select}{data.frame} (.data, ...)
}
\arguments{
  \item{.data}{a data frame}

  \item{...}{variables interpreted in the context of
  \code{.data}}
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

