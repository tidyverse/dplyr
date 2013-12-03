\name{manip_df}
\alias{filter.tbl_cpp}
\alias{manip_df}
\alias{mutate.tbl_cpp}
\alias{select.tbl_cpp}
\alias{summarise.tbl_cpp}
\title{Data manipulation for data frames.}
\usage{
\method{arrange}{tbl_cpp}(.data, ...)

\method{filter}{tbl_cpp}(.data, ...)

\method{mutate}{tbl_cpp}(.data, ...)

\method{summarise}{tbl_cpp}(.data, ...)

\method{select}{tbl_cpp}(.data, ...)
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

