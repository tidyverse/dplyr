\name{tbl_dt}
\alias{.datatable.aware}
\alias{tbl_dt}
\title{Create a data table tbl.}
\usage{
tbl_dt(data)
}
\arguments{
  \item{data}{a data table}
}
\description{
A data table tbl wraps a local data table.
}
\examples{
if (require("data.table")) {
ds <- tbl_dt(mtcars)
ds
as.data.table(ds)
as.tbl(mtcars)
}
}

