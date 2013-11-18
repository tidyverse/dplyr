\name{group_by}
\alias{group_by}
\title{Group a tbl by one or more variables.}
\usage{
group_by(x, ...)
}
\arguments{
  \item{x}{a tbl}

  \item{...}{variables to group by. All tbls accept
  variable names, some will also accept functons of
  variables.}
}
\description{
  Most data operations are useful done on groups defined by
  variables in the the dataset. The \code{group_by}
  function takes an existing tbl and converts it into a
  grouped tbl where operations are performed "by group".
}
\section{Tbl types}{
  \code{group_by} is an S3 generic with methods for the
  three built-in tbls. See the help for the corresponding
  classes and their manip methods for more details:

  \itemize{ \item data.frame: \link{grouped_df},
  \link{manip_grouped_df} \item data.table:
  \link{grouped_dt}, \link{manip_grouped_dt} \item SQLite:
  \code{\link{src_sqlite}} \item PostgreSQL:
  \code{\link{src_postgres}} \item MySQL:
  \code{\link{src_mysql}} }
}
\examples{
by_cyl <- group_by(mtcars, cyl)
summarise(by_cyl, mean(disp), mean(hp))
filter(by_cyl, disp == max(disp))
}
\seealso{
  \code{\link{ungroup}} for the inverse operation.
}

