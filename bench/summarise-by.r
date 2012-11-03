{
  baseball_df <- data_frame_source(baseball)
  baseball_dt <- data_table_source(baseball)
  baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
}

ddply(baseball, "id", summarise, g = mean(g))
#:    user  system elapsed
#:   0.499   0.003   0.503
summarise_by(baseball_s, group("id"), list(g = quote(mean(g))))
#:    user  system elapsed
#:   0.038   0.002   0.039
summarise_by(baseball_dt, group("id"), list(g = quote(mean(g))))
#:    user  system elapsed
#:   0.008   0.000   0.007
summarise_by(baseball_df, group("id"), list(g = quote(mean(g))))
#:    user  system elapsed
#:   0.028   0.000   0.029
