\name{all.equal.data.frame}
\alias{all.equal.data.frame}
\title{Provide a useful implementation of all.equal for data.frames.}
\usage{
  \method{all.equal}{data.frame} (target, current,
    ignore_col_order = TRUE, ignore_row_order = TRUE,
    convert = FALSE)
}
\arguments{
  \item{target,current}{two data frames to compare}

  \item{ignore_col_order}{should order of columns be
  ignored?}

  \item{ignore_row_order}{should order of rows be ignored?}

  \item{convert}{Should similar classes be converted?
  Currently this will convert factor to character and
  integer to double.}
}
\value{
  \code{TRUE} if equal, otherwise a character vector
  describing the first reason why they're not equal. Use
  \code{\link{isTRUE}} if using the result in an \code{if}
  expression.
}
\description{
  Provide a useful implementation of all.equal for
  data.frames.
}
\examples{
scramble <- function(x) x[sample(nrow(x)), sample(ncol(x))]

# By default, ordering of rows and columns ignored
all.equal(mtcars, scramble(mtcars))

# But those can be overriden if desired
all.equal(mtcars, scramble(mtcars), ignore_col_order = FALSE)
all.equal(mtcars, scramble(mtcars), ignore_row_order = FALSE)
}

