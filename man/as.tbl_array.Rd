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
\description{
  Coerce an existing data structure into a \code{tbl_array}
}

