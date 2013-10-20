require(dplyr, quietly = TRUE)
require(data.table, quietly = TRUE )
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

equal_ <- dplyr:::equal_

hflights_dt  <- tbl_dt(hflights)
hflights_df  <- tbl_df(hflights)
hflights_cpp <- tbl_cpp(hflights)

by_day_df  <- group_by(hflights_df , Year, Month, DayofMonth)
by_day_dt  <- group_by(hflights_dt , Year, Month, DayofMonth)
by_day_cpp <- group_by(hflights_cpp, Year, Month, DayofMonth)

stopifnot( dplyr:::all_equal_(
    summarise(by_day_df   , Distance = mean(Distance) ), 
    summarise(by_day_dt   , Distance = mean(Distance) ),
    summarise(by_day_cpp  , Distance = mean(Distance) )
) )
 
microbenchmark( 
    df  = summarise(by_day_df    , Distance = mean(Distance), min_Distance = min(Distance) ), 
    dt  = summarise(by_day_dt    , Distance = mean(Distance), min_Distance = min(Distance) ), 
    cpp = summarise(by_day_cpp   , Distance = mean(Distance), min_Distance = min(Distance) )
)

microbenchmark( 
    df  = summarise(by_day_df    , delayed = sum(ArrDelay > 0, na.rm = TRUE)), 
    dt  = summarise(by_day_dt    , delayed = sum(ArrDelay > 0, na.rm = TRUE)),
    cpp = summarise(by_day_cpp   , delayed = sum(ArrDelay > 0, na.rm = TRUE))
)
