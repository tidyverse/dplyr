\name{grouped_df}
\alias{group_by.data.frame}
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

\method{group_by}{data.frame}(x, ..., drop = TRUE)
}
\arguments{
  \item{data}{a tbl or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{lazy}{if \code{TRUE}, index will be computed lazily
  every time it is needed. If \code{FALSE}, index will be
  computed up front on object creation.}

  \item{drop}{if \code{TRUE} preserve all factor levels,
  even those without data.}

  \item{x}{object (data frame or \code{\link{tbl_df}}) to
  group}

  \item{...}{unquoted variables to group by}
}
\description{
  The easiest way to create a grouped data frame is to call
  the \code{group_by} method on a data frame or tbl: this
  will take care of capturing the unevalated expressions
  for you.
}
\keyword{internal}

