\name{group_size}
\alias{group_size}
\title{Calculate the size of each group}
\usage{
  group_size(x)
}
\arguments{
  \item{x}{a grouped tbl}
}
\description{
  Calculate the size of each group
}
\examples{
group_size(group_by(hflights, Year, Month, DayofMonth))
group_size(group_by(hflights, Dest))
}

