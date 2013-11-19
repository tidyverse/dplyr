\name{manip_df}
\alias{arrange.data.frame}
\alias{filter.data.frame}
\alias{manip_df}
\alias{mutate.data.frame}
\alias{select.data.frame}
\alias{summarise.data.frame}
\title{Data manipulation for data frames.}
\usage{
\method{filter}{data.frame}(.data, ..., env = parent.frame())

\method{summarise}{data.frame}(.data, ...)

\method{mutate}{data.frame}(.data, ...)

\method{arrange}{data.frame}(.data, ...)

\method{select}{data.frame}(.data, ...)
}
\arguments{
  \item{.data}{a data frame}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{env}{The environment in which to evaluate arguments
  not included in the data. The default should suffice for
  ordinary usage.}
}
\description{
Data manipulation for data frames.
}
\examples{
filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
head(select(hflights, Year:DayOfWeek))
summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
head(mutate(hflights, gained = ArrDelay - DepDelay))
head(arrange(hflights, Dest, desc(ArrDelay)))
}

