\name{manip_dt}
\alias{arrange.data.table}
\alias{filter.data.table}
\alias{manip_dt}
\alias{mutate.data.table}
\alias{select.data.table}
\alias{summarise.data.table}
\title{Data manipulation for data tables.}
\usage{
  \method{filter}{data.table} (.data, ...)

  \method{summarise}{data.table} (.data, ...)

  \method{mutate}{data.table} (.data, ..., inplace = FALSE)

  \method{arrange}{data.table} (.data, ...)

  \method{select}{data.table} (.data, ...)
}
\description{
  Data manipulation for data tables.
}
\examples{
if (require("data.table")) {
data("baseball", package = "plyr")

# If you start with a data table, you end up with a data table
baseball <- as.data.table(baseball)
filter(baseball, year > 2005, g > 130)
head(select(baseball, id:team))
summarise(baseball, g = mean(g), n = count())
head(mutate(baseball, rbi = r / ab, rbi2 = rbi ^ 2))
head(arrange(baseball, id, desc(year)))

# If you start with a source, you end up with a source
baseball_s <- as.source(baseball)
}
}

