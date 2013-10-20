require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(data.table, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

equal_  <- dplyr:::equal_

message( "filter( data.frame )  not grouped" )

hflights_dt  <- tbl_dt( hflights )
hflights_df  <- tbl_df( hflights )
hflights_cpp <- tbl_cpp( hflights )

microbenchmark( 
    dplyr     = filter(hflights_df , Month == 1, DayofMonth == 1, Dest == "DFW"),
    dplyr_dt  = filter(hflights_dt , Month == 1, DayofMonth == 1, Dest == "DFW"),
    internal  = filter(hflights_cpp, Month == 1, DayofMonth == 1, Dest == "DFW"),
    base      = subset( hflights, Month == 1 & DayofMonth == 1 & Dest == "DFW" )
)

message( "filter( data.frame )  grouped" )

data("baseball", package = "plyr")
baseball_df  <- tbl_df(baseball)
baseball_dt  <- tbl_dt(baseball)
baseball_cpp <- tbl_cpp(baseball)

gps_df  <- group_by( baseball_df , year, id )
gps_dt  <- group_by( baseball_dt , year, id ) 
gps_cpp <- group_by( baseball_cpp, year, id ) 

res  <- filter( gps,  g == max(g), year > 2000 )
res2 <- filter( gps,  g == max(g), year > 2000 ) 
res3 <- filter( gpdt, g == max(g), year > 2000 )

# checking results are consistent
equal_(res, res2)
equal_(res, res3)

microbenchmark( 
    internal = filter_( gps, g == max(g), year > 2000 ) , 
    dplyr    = filter( gps, g == max(g), year > 2000 ), 
    dplyr_dt = filter( gpdt, g == max(g), year > 2000 ) 
)

