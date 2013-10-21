
require(dplyr, quietly = TRUE)
require(data.table, quietly = TRUE )
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

data("Batting", package = "Lahman")
batting_dt <- data.table(Batting)
setkey(batting_dt, playerID)
players_df <- group_by(Batting, playerID)
players_dt <- group_by(tbl_dt(Batting), playerID)

summarise_ <- dplyr:::summarise_
equal_ <- dplyr:::equal_

microbenchmark(
  dplyr_df  = summarise(players_df, AB = mean(AB), n = n()), 
  dplyr_dt  = summarise(players_dt, AB = mean(AB), n = n()),
  dt_raw    = batting_dt[, list(.N, mean(AB)), by = playerID],
  dplyr_Cpp = summarise_(players_df, AB = mean(AB), n = n()), 
  times = 10
)
  

microbenchmark( 
    internal = summarise_(by_day   , delayed = sum(ArrDelay > 0, na.rm = TRUE)), 
    dplyr    = summarise(by_day    , delayed = sum(ArrDelay > 0, na.rm = TRUE)), 
    dplyr_dt = summarise(by_day_dt , delayed = sum(ArrDelay > 0, na.rm = TRUE))
)
