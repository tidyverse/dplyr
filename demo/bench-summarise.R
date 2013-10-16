require(dplyr, quietly = TRUE)
require(data.table, quietly = TRUE )
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

summarise_ <- dplyr:::summarise_
equal_ <- dplyr:::equal_

hflights_dt <- data.table(hflights)
by_day <- group_by(hflights, Year, Month, DayofMonth)
by_day_dt <- group_by(hflights_dt, Year, Month, DayofMonth)

by_month     <- summarise_(by_day   , Distance = mean(Distance) )
by_month_int <- summarise(by_day    , Distance = mean(Distance) )
stopifnot( equal_( by_month, by_month_int ) )
 
microbenchmark( 
    internal = summarise_(by_day   , Distance = mean(Distance), min_Distance = min(Distance) ), 
    dplyr    = summarise(by_day    , Distance = mean(Distance), min_Distance = min(Distance) ), 
    dplyr_dt = summarise(by_day_dt , Distance = mean(Distance), min_Distance = min(Distance) )
)
q("no")

microbenchmark( 
    internal = summarise_(by_day   , delayed = sum(ArrDelay > 0, na.rm = TRUE)), 
    dplyr    = summarise(by_day    , delayed = sum(ArrDelay > 0, na.rm = TRUE)), 
    dplyr_dt = summarise(by_day_dt , delayed = sum(ArrDelay > 0, na.rm = TRUE))
)
