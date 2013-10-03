\name{copy_to}
\alias{copy_to}
\title{Copy a local data frame to a remote src.}
\usage{
  copy_to(dest, df, name = deparse(substitute(df)), ...)
}
\arguments{
  \item{dest}{remote data source}

  \item{df}{local data frame}

  \item{name}{name for new remote table.}

  \item{...}{other parameters passed to methods.}
}
\value{
  a \code{tbl} object in the remote source
}
\description{
  This uploads a local data frame into a remote data
  source, creating the table definition as needed. Wherever
  possible, the new object will be temporary, limited to
  the current connection to the source.
}

