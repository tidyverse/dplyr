\name{temp_srcs}
\alias{temp_load}
\alias{temp_srcs}
\title{Connect to temporary data sources.}
\usage{
  temp_srcs(...)

  temp_load(srcs, df, name = random_table_name())
}
\description{
  These functions make it easy to take a local data frame
  and make available as a tbl in every known src. All local
  srcs will work on any computer. DBMS srcs will only
  currently work on Hadley's computer.
}
\examples{
\dontrun{
local <- c("df", "dt", "cpp")
db <- c("sqlite", "mysql", "postgres")

temp_srcs(local)
temp_srcs(db)

mtcars_tbls <- temp_tbls(local, mtcars)
}
}
\keyword{internal}

