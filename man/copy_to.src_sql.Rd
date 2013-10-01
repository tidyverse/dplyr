\name{copy_to.src_sql}
\alias{copy_to.src_sql}
\title{Copy a local data fram to a sqlite src.}
\usage{
  \method{copy_to}{src_sql} (dest, df,
    name = deparse(substitute(df)), types = NULL,
    temporary = TRUE, indexes = NULL, analyze = TRUE, ...)
}
\arguments{
  \item{types}{a character vector giving variable types to
  use for the columns.  See
  \url{http://www.sqlite.org/datatype3.html} for available
  types.}

  \item{temporary}{if \code{TRUE}, will create a temporary
  table that is local to this connection and will be
  automatically deleted when the connection expires}

  \item{indexes}{}
}
\value{
  a \code{\link{tbl_sqlite}} object
}
\description{
  Copy a local data fram to a sqlite src.
}
\examples{
db <- src_sqlite(tempfile(), create = TRUE)

iris2 <- copy_to(db, iris)
mtcars$model <- rownames(mtcars)
mtcars2 <- copy_to(db, mtcars, indexes = list("model"))

explain_tbl(filter(mtcars2, model == "Hornet 4 Drive"))

# Note that tables are temporary by default, so they're not
# visible from other connections to the same database.
src_tbls(db)
db2 <- src_sqlite(db$path)
src_tbls(db2)
}

