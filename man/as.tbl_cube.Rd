\name{as.tbl_cube}
\alias{as.tbl_cube}
\alias{as.tbl_cube.array}
\alias{as.tbl_cube.data.frame}
\alias{as.tbl_cube.table}
\title{Coerce an existing data structure into a \code{tbl_cube}}
\usage{
as.tbl_cube(x, ...)

\method{as.tbl_cube}{array}(x, met_name = deparse(substitute(x)),
  dim_names = names(dimnames(x)), ...)

\method{as.tbl_cube}{table}(x, met_name = deparse(substitute(x)),
  dim_names = names(dimnames(x)), ...)

\method{as.tbl_cube}{data.frame}(x, dim_names, ...)
}
\arguments{
  \item{x}{an object to convert. Built in methods will
  convert arrays, tables and data frames.}

  \item{...}{Passed on to individual methods; otherwise
  ignored.}

  \item{met_name}{a string to use as the name for the
  metric}

  \item{dim_names}{names of the dimesions. Defaults to the
  names of the \code{\link{dimnames}}.}
}
\description{
Coerce an existing data structure into a \code{tbl_cube}
}

