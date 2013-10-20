require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(data.table, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

all_equal_  <- dplyr:::all_equal_

message( "filter( data.frame )  not grouped" )

hflights_dt  <- tbl_dt( hflights )
hflights_df  <- tbl_df( hflights )
hflights_cpp <- tbl_cpp( hflights )

microbenchmark( 
    df     = filter(hflights_df , Month == 1, DayofMonth == 1, Dest == "DFW"),
    dt     = filter(hflights_dt , Month == 1, DayofMonth == 1, Dest == "DFW"),
    cpp    = filter(hflights_cpp, Month == 1, DayofMonth == 1, Dest == "DFW"),
    base   = subset(hflights, Month == 1 & DayofMonth == 1 & Dest == "DFW" )
)

message( "filter( data.frame )  grouped" )

data("baseball", package = "plyr")
baseball_df  <- tbl_df(baseball)
baseball_dt  <- tbl_dt(baseball)
baseball_cpp <- tbl_cpp(baseball)

gps_df  <- group_by( baseball_df , year, id )
gps_dt  <- group_by( baseball_dt , year, id ) 
gps_cpp <- group_by( baseball_cpp, year, id ) 
class(gps_cpp)

res_df  <- filter( gps_df , g == max(g), year > 2000 )
res_dt  <- filter( gps_dt , g == max(g), year > 2000 ) 
res_cpp <- filter( gps_cpp, g == max(g), year > 2000 )
all_equal_(res_df, res_dt, res_cpp)

microbenchmark( 
    cpp = filter( gps_cpp, g == max(g), year > 2000 ), 
    df  = filter( gps_df , g == max(g), year > 2000 ), 
    dt  = filter( gps_dt , g == max(g), year > 2000 ) 
)

