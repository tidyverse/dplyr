\name{join.tbl_array}
\alias{inner_join.tbl_array}
\alias{join.tbl_array}
\title{Join \code{tbl_array}s.}
\usage{
  \method{inner_join}{tbl_array} (x, y, by = NULL,
    copy = FALSE, ...)
}
\description{
  Join \code{tbl_array}s.
}
\examples{
by_loc <- group_by(nasa, lat, long)
temp <- summarise(by_loc, avg_temp = mean(temperature))
inner_join(by_loc, temp)
}

