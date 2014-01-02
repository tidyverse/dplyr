\name{manip_df}
\alias{arrange.tbl_df}
\alias{filter.tbl_df}
\alias{manip_df}
\alias{mutate.tbl_df}
\alias{select.tbl_df}
\alias{summarise.tbl_df}
\title{Data manipulation for data frames.}
\usage{
\method{arrange}{tbl_df}(.data, ...)

\method{filter}{tbl_df}(.data, ...)

\method{mutate}{tbl_df}(.data, ...)

\method{summarise}{tbl_df}(.data, ...)

\method{select}{tbl_df}(.data, ...)
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
if (require("hflights")) {
filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
head(select(hflights, Year:DayOfWeek))
summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
head(mutate(hflights, gained = ArrDelay - DepDelay))
head(arrange(hflights, Dest, desc(ArrDelay)))
}
}

