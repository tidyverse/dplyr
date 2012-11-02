{
  baseball_df <- data_frame_source(baseball)
  baseball_dt <- data_table_source(baseball)
  baseball_s <- sqlite_source("inst/db/baseball.sqlite3", "baseball")
}

summarise_by(baseball_s, group("id"), list(g = quote(mean(g))))
summarise_by(baseball_dt, group("id"), list(g = quote(mean(g))))
summarise_by(baseball_df, group("id"), list(g = quote(mean(g))))
