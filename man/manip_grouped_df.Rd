\name{manip_grouped_df}
\alias{arrange.grouped_df}
\alias{filter.grouped_df}
\alias{manip_grouped_df}
\alias{mutate.grouped_df}
\alias{select.grouped_df}
\alias{summarise.grouped_df}
\title{Data manipulation for grouped data frames.}
\usage{
\method{filter}{grouped_df}(.data, ...)

\method{summarise}{grouped_df}(.data, ...)

\method{mutate}{grouped_df}(.data, ...)

\method{arrange}{grouped_df}(.data, ...)

\method{select}{grouped_df}(.data, ...)
}
\arguments{
  \item{.data}{a data frame}

  \item{...}{variables interpreted in the context of
  \code{.data}}
}
\description{
Grouping status is preserved across filter, arrange, mutate
and select. Summarise produces an ungrouped data frame.
}
\examples{
by_dest <- group_by(hflights, Dest)
filter(by_dest, ArrDelay == max(ArrDelay))
summarise(by_dest, arr = mean(ArrDelay, na.rm = TRUE))

# Normalise arrival and departure delays by airport
scaled <- mutate(by_dest, arr_z = scale(ArrDelay), dep_z = scale(DepDelay))
select(scaled, Year:DayOfWeek, Dest, arr_z:dep_z)

arrange(by_dest, desc(ArrDelay))
select(by_dest, -(DayOfWeek:TailNum))

# All manip functions preserve grouping structure, except for summarise
# which removes a grouping level
by_day <- group_by(hflights, Year, Month, DayofMonth)
by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
by_month
summarise(by_month, delayed = sum(delayed))

# You can also manually ungroup:
ungroup(by_day)
}

