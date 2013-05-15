\name{manip_grouped_dt}
\alias{arrange.grouped_dt}
\alias{filter.grouped_dt}
\alias{manip_grouped_dt}
\alias{mutate.grouped_dt}
\alias{select.grouped_dt}
\alias{summarise.grouped_dt}
\title{Data manipulation for grouped data tables.}
\usage{
  \method{filter}{grouped_dt} (.data, ...)

  \method{summarise}{grouped_dt} (.data, ...)

  \method{mutate}{grouped_dt} (.data, ..., inplace = FALSE)

  \method{arrange}{grouped_dt} (.data, ...)

  \method{select}{grouped_dt} (.data, ...)
}
\arguments{
  \item{.data}{a data table}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{inplace}{if \code{FALSE} (the default) the data
  frame will be copied prior to modification to avoid
  changes propagating via reference.}
}
\description{
  Data manipulation for grouped data tables.
}
\examples{
if (require("data.table")) {
data("baseball", package = "plyr")
baseball_dt <- as.data.table(baseball)
players <- group_by(baseball_dt, id)
filter(players, g == max(g))
summarise(players, g = mean(g))
mutate(players, cyear = year - min(year) + 1)
arrange(players, id, desc(year))
select(players, id:team)

# All manip functions preserve grouping structure, except for summarise
# (for hopefully obvious reasons)
by_year <- mutate(players, cyear = year - min(year) + 1)
summarise(by_year, years = max(cyear))

# You can also manually ungroup:
arrange(ungroup(by_year), id, year)
}
}

