\name{group_size}
\alias{group_size}
\title{Calculate the size of each group}
\usage{
  group_size(x)
}
\arguments{
  \item{x}{a grouped data source}
}
\description{
  Calculate the size of each group
}
\examples{
data("baseball", package = "plyr")
players_df <- group_by(baseball, id)
group_size(players_df)
}

