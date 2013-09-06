\name{semi_join}
\alias{semi_join}
\title{Semi joins and anti joins.}
\usage{
  semi_join(x, y, by = NULL, anti = FALSE, copy = FALSE,
    ...)
}
\arguments{
  \item{anti}{If \code{TRUE}, performs an anti join instead
  of a semi join.}

  \item{by}{a character vector of variables to join by.  If
  \code{NULL}, the default, \code{join} will do a natural
  join, using all variables with common names across the
  two tables. A message lists the variables so that you can
  check they're right.}

  \item{copy}{If \code{x} and \code{y} are not from the
  same data source, and \code{copy} is \code{TRUE}, then
  \code{y} will be copied into the same src as \code{x}.
  This allows you to join tables across srcs, but it is a
  potentially expensive operation so you must opt into it.}

  \item{...}{other parameters passed onto individual
  methods}
}
\description{
  A semi join keeps all records from \code{x} that have
  matching rows in \code{y}. It preserves all columns from
  \code{x}, but none from \code{y}
}

