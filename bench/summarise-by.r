{
  library(plyr)

  baseball_df <- data_frame_source(baseball)
  baseball_dt <- data_table_source(baseball)
  baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
}

ddply(baseball, "id", summarise, g = mean(g))
#:    user  system elapsed
#:   0.471   0.002   0.473
summarise_by(baseball_s, "id", g = mean(g))
#:    user  system elapsed
#:   0.037   0.002   0.039
summarise_by(baseball_dt, "id", g = mean(g))
#:    user  system elapsed
#:   0.007   0.000   0.007
summarise_by(baseball_df, "id", g = mean(g))
#:    user  system elapsed
#:   0.028   0.000   0.028
aggregate(g ~ id, baseball, mean)
#:    user  system elapsed
#:   0.040   0.002   0.041
