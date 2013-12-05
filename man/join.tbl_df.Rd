\name{join.tbl_df}
\alias{anti_join.tbl_cpp}
\alias{inner_join.tbl_cpp}
\alias{join.tbl_df}
\alias{left_join.tbl_cpp}
\alias{semi_join.tbl_cpp}
\title{Join data table tbls.}
\usage{
\method{inner_join}{tbl_cpp}(x, y, by = NULL, copy = FALSE, ...)

\method{left_join}{tbl_cpp}(x, y, by = NULL, copy = FALSE, ...)

\method{semi_join}{tbl_cpp}(x, y, by = NULL, copy = FALSE, ...)

\method{anti_join}{tbl_cpp}(x, y, by = NULL, copy = FALSE, ...)
}
\arguments{
  \item{x,y}{tbls to join}

  \item{by}{a character vector of variables to join by.  If
  \code{NULL}, the default, \code{join} will do a natural
  join, using all variables with common names across the
  two tables. A message lists the variables so that you can
  check they're right - to suppress the message, supply a
  character vector.}

  \item{copy}{If \code{y} is not a data frame or
  \code{\link{tbl_df}} and \code{copy} is \code{TRUE},
  \code{y} will be converted into a data frame}

  \item{...}{included for compatibility with the generic;
  otherwise ignored.}
}
\description{
See \code{\link{join}} for a description of the general
purpose of the functions. The data frame implementations
are currently not terribly efficient.
}
\examples{
data("Batting", package = "Lahman")
data("Master", package = "Lahman")

batting_df <- tbl_df(Batting)
person_df <- tbl_df(Master)

uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])

# Inner join: match batting and person data
inner_join(batting_df, person_df)
inner_join(batting_df, uperson_df)

# Left join: match, but preserve batting data
left_join(batting_df, uperson_df)

# Anti join: find batters without person data
anti_join(batting_df, person_df)
# or people who didn't bat
anti_join(person_df, batting_df)
}

