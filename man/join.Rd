\name{join}
\alias{join}
\title{Join two tbls together}
\usage{
  join(x, y, by = NULL, type = "left", copy = FALSE, ...)
}
\arguments{
  \item{x,y}{tbls to join}

  \item{by}{a character vector of variables to join by.  If
  \code{NULL}, the default, \code{join} will do a natural
  join, using all variables with common names across the
  two tables. A message lists the variables so that you can
  check they're right.}

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
  Groups are ignored for the purpose of joining, but the
  result preserves the grouping of \code{x}.
}

