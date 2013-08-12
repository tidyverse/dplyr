\name{grouped_dt}
\alias{group_by.data.table}
\alias{group_by.source_dt}
\alias{grouped_dt}
\alias{is.grouped_dt}
\title{A grouped data table.}
\usage{
  grouped_dt(data, vars)

  is.grouped_dt(x)

  \method{group_by}{data.table} (x, ...)

  \method{group_by}{source_dt} (x, ...)
}
\arguments{
  \item{data}{a data source or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{x}{an object to check}

  \item{...}{variables to group by}
}
\description{
  The easiest way to create a grouped data table is to call
  the \code{group_by} method on a data table or data
  source: this will take care of capturing the unevalated
  expressions for you.
}
\examples{
data("baseball", package = "plyr")
by_player <- group_by(data.table(baseball), id)
group_size(by_player)
}

