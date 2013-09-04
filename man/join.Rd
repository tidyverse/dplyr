\name{join}
\alias{join}
\title{Join two tbls together}
\usage{
  join(x, y, by = NULL, type = "left", copy = FALSE, ...)
}
\arguments{
  \item{x,y}{tbls to join}

  \item{by}{a character vector of variables to join by}

  \item{type}{a string giving the join type. Possible
  values are left (the default), right, full and inner. Not
  all types will be supported by all tbls.}

  \item{copy}{If \code{x} and \code{y} are not from the
  same data source, and \code{copy} is \code{TRUE}, then
  \code{y} will be copied into the same src as \code{x}.
  This allows you to join tables across srcs, but it is a
  potentially expensive operation so you must opt into it.}

  \item{...}{other parameters passed onto individual
  methods}
}
\description{
  Join two tbls together
}

