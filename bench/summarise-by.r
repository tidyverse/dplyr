{
  library(plyr)

  baseball_df <- source_df(baseball)
  baseball_dt <- source_dt(baseball)
  baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
}

ddply(baseball, "id", summarise, avg_games = mean(g))
#:    user  system elapsed
#:   0.471   0.002   0.473
summarise_by(baseball_s, "id", avg_games = mean(g))
#:    user  system elapsed
#:   0.037   0.002   0.039
summarise_by(baseball_dt, "id", avg_games = mean(g))
#:    user  system elapsed
#:   0.007   0.000   0.007
summarise_by(baseball_df, "id", avg_games = mean(g))
#:    user  system elapsed
#:   0.028   0.000   0.028
aggregate(g ~ id, baseball, mean)
#:    user  system elapsed
#:   0.040   0.002   0.041
