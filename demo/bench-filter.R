require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(data.table, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

filter_ <- dplyr:::filter_
equal_  <- dplyr:::equal_

message( "filter( data.frame )  not grouped" )

hflights_dt <- tbl_dt( hflights )
hflights_df <- tbl_df( hflights )
microbenchmark( 
    dplyr     = filter(hflights_df, Month == 1, DayofMonth == 1, Dest == "DFW"),
    dplyr_dt  = filter(hflights_dt, Month == 1, DayofMonth == 1, Dest == "DFW"),
    internal  = filter_(hflights, Month == 1, DayofMonth == 1, Dest == "DFW"),
    base      = subset( hflights, Month == 1 & DayofMonth == 1 & Dest == "DFW" )
)

message( "filter( data.frame )  grouped" )

data("baseball", package = "plyr")
gps  <- group_by( baseball, year, id )
gpdt <- group_by( tbl_dt( baseball), year, id ) 
res  <- filter_( gps,  g == max(g), year > 2000 )
res2 <- filter(  gps,  g == max(g), year > 2000 ) 
res3 <- filter(  gpdt, g == max(g), year > 2000 )

# checking results are consistent
equal_(res, res2)
equal_(res, res3)

microbenchmark( 
    internal = filter_( gps, g == max(g), year > 2000 ) , 
    dplyr    = filter( gps, g == max(g), year > 2000 ), 
    dplyr_dt = filter( gpdt, g == max(g), year > 2000 ) 
)

