\name{manip_source}
\alias{arrange.source}
\alias{filter.source}
\alias{group.source}
\alias{manip_source}
\alias{mutate.source}
\alias{select.source}
\alias{summarise.source}
\title{Data manipulation for data source objects.}
\usage{
  \method{filter}{source} (_data, ...,
    _env = parent.frame())

  \method{summarise}{source} (_data, ...,
    _env = parent.frame())

  \method{mutate}{source} (_data, ...,
    _env = parent.frame())

  \method{arrange}{source} (_data, ...,
    _env = parent.frame())

  \method{select}{source} (_data, ...,
    _env = parent.frame())

  \method{group}{source} (_data, ...,
    _env = parent.frame())
}
\description{
  All data manipulation on data source objects is lazy: all
  functions return a lightweight object representing the
  action. This is only turned into an result when
  \code{\link{render}} is called.
}
\examples{
data("baseball", package = "plyr")
bdf <- data_frame_source(baseball)
path <- "inst/db/baseball.sqlite3"
baseball_s <- sqlite_source(path, "baseball")
filter(baseball_s, year > 1980)
summarise(bdf, g = mean(g))
mutate(bdf, cyear = year - min(year) + 1)
arrange(bdf, id, desc(year))

recent <- filter(baseball_s, year > 2005)
render(select(recent, id:lg, rbi))
render(summarise(recent, g = mean(g)))
render(mutate(recent, year - max(year)))
}

