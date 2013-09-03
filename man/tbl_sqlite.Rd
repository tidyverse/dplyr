\name{tbl_sqlite}
\alias{tbl.src_sqlite}
\alias{tbl_sqlite}
\title{Create an sqlite tbl.}
\usage{
  tbl_sqlite(path, table)

  \method{tbl}{src_sqlite} (src, table, ...)
}
\arguments{
  \item{path,src}{either path to sqlite database, or
  \code{src_sqlite} object}

  \item{table}{name of table in database}

  \item{...}{other arguments ignored, but needed for
  compatibility with generic.}
}
\description{
  You can create a sqlite tbl with a table name and a path
  or \code{\link{src_sqlite}} object. You need to use
  \code{src_sqlite} if you're working with multiple tables
  from the same database so that they use the same
  connection, and so can perform joins etc.
}
\details{
  To see exactly what SQL is being sent to the database,
  you can set option \code{dplyr.show_sql} to true:
  \code{options(dplyr.show_sql = TRUE).} If you're
  wondering why a particularly query is slow, it can be
  helpful to see the query plan. You can do this by setting
  \code{options(dplyr.explain_sql = TRUE)}. The output of
  SQLite's query plans is relatively easy to make sense of
  and is explained at \url{http://www.sqlite.org/eqp.html}.
  You may also find the explanation of how SQL indices
  works to be helpful:
  \url{http://www.sqlite.org/queryplanner.html}.
}
\examples{
# You can create from a path and a table name
db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_s <- tbl_sqlite(db_path, "baseball")

# Or (better) from a sqlite src and a table name
db <- src_sqlite(db_path)
baseball_s <- tbl(db, "baseball")

dim(baseball_s)
names(baseball_s)
head(baseball_s)

players <- group_by(baseball_s, id)
summarise(players, g = mean(g), n = count())
}

