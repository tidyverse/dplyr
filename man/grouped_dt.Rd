\name{grouped_dt}
\alias{grouped_dt}
\alias{is.grouped_dt}
\title{A grouped data table.}
\usage{
grouped_dt(data, vars)

is.grouped_dt(x)
}
\arguments{
  \item{data}{a tbl or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{x}{an object to check}
}
\description{
The easiest way to create a grouped data table is to call
the \code{group_by} method on a data table or tbl: this
will take care of capturing the unevalated expressions for
you.
}
\examples{
if (require("hflights")) {
hflights_dt <- tbl_dt(hflights)
group_size(group_by(hflights_dt, Year, Month, DayofMonth))
group_size(group_by(hflights_dt, Dest))

monthly <- group_by(hflights_dt, Month)
summarise(monthly, n = n(), delay = mean(ArrDelay))
}
}

