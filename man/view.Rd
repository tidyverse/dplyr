\name{view}
\alias{view}
\title{data(baseball, package = "plyr")
players <- group_by(baseball, id)
v <- view(players$obj, players$index)
v$set_group(1)
v$eval(quote(id))}
\usage{
  view(data, index, parent = parent.frame())
}
\description{
  data(baseball, package = "plyr") players <-
  group_by(baseball, id) v <- view(players$obj,
  players$index) v$set_group(1) v$eval(quote(id))
}

