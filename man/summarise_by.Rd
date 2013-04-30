\name{summarise_by}
\alias{summarise_by}
\title{Summarise by.}
\usage{
  summarise_by(.source, .group, ..., .n = 1e+05,
    .env = parent.frame())
}
\arguments{
  \item{.source}{data source.  See \code{\link{source}} for
  more details.}

  \item{.group}{grouping variable or variables. Either a
  character vector, a formula, or the output of
  \code{\link[plyr]{.}}.  Use \code{NULL} for no grouping.}

  \item{...}{additional named arguments are processed in
  the context of the \code{source}.}

  \item{.env}{the environment in which to look for any
  additional variables supplied to the calls in
  \code{...}.}
}
\description{
  Summarise variables in a data source broken down by some
  grouping variable.
}
\section{Restrictions}{
  \itemize{ \item summary functions must return single
  value \item summary functions must return the same type
  of value for all groups }
}

