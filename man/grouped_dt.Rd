\name{grouped_dt}
\alias{arrange.grouped_dt}
\alias{filter.grouped_dt}
\alias{group_by.data.table}
\alias{group_by.source_dt}
\alias{grouped_dt}
\alias{is.grouped_dt}
\alias{mutate.grouped_dt}
\alias{select.grouped_dt}
\alias{summarise.grouped_dt}
\title{A grouped data table.}
\usage{
  grouped_dt(data, vars, name = NULL)

  is.grouped_dt(x)

  \method{group_by}{data.table} (x, ..., name = NULL)

  \method{group_by}{source_dt} (x, ...)

  \method{filter}{grouped_dt} (.data, ...)

  \method{summarise}{grouped_dt} (.data, ...)

  \method{mutate}{grouped_dt} (.data, ..., inplace = FALSE)

  \method{arrange}{grouped_dt} (.data, ...)

  \method{select}{grouped_dt} (.data, ...)
}
\arguments{
  \item{data}{a data source or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{name}{data source name.}
}
\description{
  The easiest way to create a grouped data table is to call
  the \code{group_by} method on a data table or data
  source: this will take care of capturing the unevalated
  expressions for you.

  Data manipulation for grouped data tables.
}
\examples{
data("baseball", package = "plyr")
baseball_dt <- as.data.table(baseball)
players <- group_by(baseball_dt, id, name = "players")
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

