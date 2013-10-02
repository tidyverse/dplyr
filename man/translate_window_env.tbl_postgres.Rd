\name{translate_window_env.tbl_postgres}
\alias{translate_window_env.tbl_postgres}
\title{A sql variant for translating windowed PostgreSQL functions.}
\usage{
  translate_window_env.tbl_postgres(x)
}
\arguments{
  \item{variables}{to group by (goes into \code{PARTITION
  BY})}
}
\description{
  A sql variant for translating windowed PostgreSQL
  functions.
}
\examples{
by_team <- sql_windowed(list(quote(TeamID)))
# Show all functions:
ls(by_team)

translate_sql(variant = by_team,
   n(),
   mean(AB),
   cumsum(AB, YearID),
   order(),
   rank(YearID)
)
}

