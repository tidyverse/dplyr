\name{tbl_cpp}
\alias{tbl_cpp}
\title{Create a data frame tbl with cpp backend.}
\usage{
tbl_cpp(data)
}
\arguments{
  \item{data}{a data frame}
}
\description{
Create a data frame tbl with cpp backend.
}
\examples{
ds <- tbl_cpp(mtcars)
ds
as.data.frame(ds)

library(Lahman)
batting <- tbl_cpp(Batting)
dim(batting)
colnames(batting)
head(batting)

# Data manipulation verbs ---------------------------------------------------
filter(batting, yearID > 2005, G > 130)
select(batting, playerID:lgID)
arrange(batting, playerID, desc(yearID))
summarise(batting, G = mean(G), n = n())
mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)

# Group by operations -------------------------------------------------------
# To perform operations by group, create a grouped object with group_by
players <- group_by(batting, playerID)
head(group_size(players), 100)

summarise(players, mean_g = mean(G), best_ab = max(AB))
best_year <- filter(players, AB == max(AB) || G == max(G))
progress <- mutate(players, cyear = yearID - min(yearID) + 1,
 rank(desc(AB)), cumsum(AB))

# When you group by multiple level, each summarise peels off one level
per_year <- group_by(batting, playerID, yearID)
stints <- summarise(per_year, stints = max(stint))
# filter(stints, stints > 3)
# summarise(stints, max(stints))
# mutate(stints, cumsum(stints))

# Joins ---------------------------------------------------------------------
player_info <- select(tbl_cpp(Master), playerID, hofID, birthYear)
hof <- select(filter(tbl_cpp(HallOfFame), inducted == "Y"),
 hofID, votedBy, category)

# Match players and their hall of fame data
inner_join(player_info, hof)
# Keep all players, match hof data where available
left_join(player_info, hof)
# Find only players in hof
semi_join(player_info, hof)
# Find players not in hof
anti_join(player_info, hof)
}

