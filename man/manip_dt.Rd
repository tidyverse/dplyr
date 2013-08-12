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
\arguments{
  \item{.data}{a data table}

  \item{...}{variables interpreted in the context of
  \code{.data}}

  \item{inplace}{if \code{FALSE} (the default) the data
  frame will be copied prior to modification to avoid
  changes propagating via reference.}
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
summarise(baseball, g = mean(g))
head(mutate(baseball, rbi = r / ab, rbi2 = rbi ^ 2))
head(arrange(baseball, id, desc(year)))

# If you start with a source, you end up with a source
baseball_s <- as.source(baseball)
filter(baseball_s, year > 2005, g > 130)
select(baseball_s, id:team)
summarise(baseball_s, g = mean(g))
mutate(baseball_s, rbi = r / ab, rbi2 = rbi ^ 2)
arrange(baseball_s, id, desc(year))
}
}

