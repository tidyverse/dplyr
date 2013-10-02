# Benchmark relatively simple join between postgre and sqlite
bench <- function(x) {
  force(x)
  replicate(3, system.time(collect(x)))[1:3, ]
}

batting1 <- tbl(lahman_postgres(), "Batting")
batting2 <- tbl(lahman_sqlite(), "Batting")

team_info1 <- select(tbl(lahman_postgres(), "Teams"), yearID, lgID, teamID, G, R:H)
team_info2 <- select(tbl(lahman_sqlite(), "Teams"), yearID, lgID, teamID, G, R:H)

first_stint1 <- select(filter(batting1, stint == 1), playerID:H)
first_stint2 <- select(filter(batting2, stint == 1), playerID:H)

both1 <- left_join(first_stint1, team_info1, by = c("yearID", "teamID", "lgID"))
both2 <- left_join(first_stint2, team_info2, by = c("yearID", "teamID", "lgID"))

bench(both1)
# user.self 0.079 0.079 0.079
# sys.self  0.013 0.012 0.012
# elapsed   0.191 0.192 0.189
bench(both2)
# user.self 0.241 0.244 0.242
# sys.self  0.015 0.015 0.014
# elapsed   0.257 0.259 0.257