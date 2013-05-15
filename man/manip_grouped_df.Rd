\name{manip_grouped_df}
\alias{arrange.grouped_df}
\alias{filter.grouped_df}
\alias{manip_grouped_df}
\alias{mutate.grouped_df}
\alias{select.grouped_df}
\alias{summarise.grouped_df}
\title{Data manipulation for grouped data frames.}
\usage{
  \method{filter}{grouped_df} (.data, ...)

  \method{summarise}{grouped_df} (.data, ...)

  \method{mutate}{grouped_df} (.data, ...)

  \method{arrange}{grouped_df} (.data, ...)

  \method{select}{grouped_df} (.data, ...)
}
\description{
  Grouping status is preserved across filter, arrange,
  mutate and select. Summarise produces an ungrouped data
  frame.
}
\examples{
data("baseball", package = "plyr")
players <- group_by(baseball, id, name = "players")
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

