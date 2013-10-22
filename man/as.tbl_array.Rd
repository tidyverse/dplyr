\name{as.tbl_array}
\alias{as.tbl_array}
\alias{as.tbl_array.array}
\alias{as.tbl_array.data.frame}
\alias{as.tbl_array.table}
\title{Coerce an existing data structure into a \code{tbl_array}}
\usage{
  as.tbl_array(x, ...)

  \method{as.tbl_array}{array} (x,
    met_name = deparse(substitute(x)),
    dim_names = names(dimnames(x)), ...)

  \method{as.tbl_array}{table} (x,
    met_name = deparse(substitute(x)),
    dim_names = names(dimnames(x)), ...)

  \method{as.tbl_array}{data.frame} (x, dim_names, ...)
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
  Coerce an existing data structure into a \code{tbl_array}
}

