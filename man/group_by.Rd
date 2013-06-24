\name{group_by}
\alias{group_by}
\title{Group a data source by one or more variables.}
\usage{
  group_by(x, ...)
}
\arguments{
  \item{x}{a data source}

  \item{...}{variables to group by. All data sources accept
  variable names, some will also accept functons of
  variables.}
}
\description{
  Most data operations are useful done on groups defined by
  variables in the the dataset. The \code{group_by}
  function takes an existing data source and converts it
  into a grouped data source where operations are performed
  "by group".
}
\section{Data sources}{
  \code{group_by} is an S3 generic with methods for the
  three built-in data sources. See the help for the
  corresponding classes and their manip methods for more
  details:

  \itemize{ \item data.frame: \link{grouped_df},
  \link{manip_grouped_df} \item data.table:
  \link{grouped_dt}, \link{manip_grouped_dt} \item SQLite:
  \link{grouped_sqlite}, \link{manip_grouped_sqlite} }
}
\examples{
by_cyl <- group_by(mtcars, cyl)
summarise(by_cyl, mean(disp), mean(hp))
filter(by_cyl, disp == max(disp))
}
\seealso{
  \code{\link{ungroup}} for the inverse operation.
}

