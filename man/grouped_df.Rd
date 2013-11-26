\name{grouped_df}
\alias{grouped_df}
\alias{is.grouped_cpp}
\alias{is.grouped_df}
\alias{is.lazy.grouped_df}
\title{A grouped data frame.}
\usage{
grouped_df(data, vars, lazy = TRUE, drop = TRUE)

\method{is.lazy}{grouped_df}(x)

is.grouped_df(x)

is.grouped_cpp(x)
}
\arguments{
  \item{data}{a tbl or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{lazy}{if \code{TRUE}, index will be computed lazily
  every time it is needed. If \code{FALSE}, index will be
  computed up front on object creation.}

  \item{drop}{if \code{TRUE} preserve all factor levels,
  even those without data.}
}
\description{
The easiest way to create a grouped data frame is to call
the \code{group_by} method on a data frame or tbl: this
will take care of capturing the unevalated expressions for
you.
}
\keyword{internal}

