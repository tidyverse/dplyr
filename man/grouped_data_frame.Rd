\name{grouped_data_frame}
\alias{group_by.data.frame}
\alias{grouped_data_frame}
\alias{is.grouped_data_frame}
\alias{is.lazy.grouped_data_frame}
\title{A grouped data frame.}
\usage{
  grouped_data_frame(data, vars, lazy = TRUE, drop = TRUE,
    name = NULL)

  \method{is.lazy}{grouped_data_frame} (x)

  is.grouped_data_frame(x)

  \method{group_by}{data.frame} (x, ..., drop = TRUE,
    name = NULL)
}
\arguments{
  \item{data}{a data source or data frame.}

  \item{vars}{a list of quoted variables.}

  \item{lazy}{if \code{TRUE}, index will be computed lazily
  every time it is needed. If \code{FALSE}, index will be
  computed up front on object creation.}

  \item{drop}{if \code{TRUE} preserve all factor levels,
  even those without data.}

  \item{name}{data source name.}
}
\description{
  The easiest way to create a grouped data frame is to call
  the \code{group_by} method on a data frame or data
  source: this will take care of capturing the unevalated
  expressions for you.
}

