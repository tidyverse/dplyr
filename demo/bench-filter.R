require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(data.table, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

strip <- function(x, order = FALSE) {
  x <- as.data.frame(x)
  if (order) x <- x[do.call("order", x), , drop = FALSE]
  rownames(x) <- NULL
  attr( x, "vars") <- NULL
  x
}

filter_ <- dplyr:::filter_
filter_grouped_df <- dplyr:::filter_grouped_df

message( "filter( data.frame )  not grouped" )

hflights_dt <- tbl_dt( hflights )
hflights_df <- tbl_df( hflights )
microbenchmark( 
    dplyr     = filter(hflights_df, Month == 1, DayofMonth == 1, Dest == "DFW"),
    dplyr_dt  = filter(hflights_dt, Month == 1, DayofMonth == 1, Dest == "DFW"),
    dplyrRcpp = filter_(hflights, Month == 1, DayofMonth == 1, Dest == "DFW"),
    base   = subset( hflights, Month == 1 & DayofMonth == 1 & Dest == "DFW" )
)

message( "filter( data.frame )  grouped" )

data("baseball", package = "plyr")
gps  <- group_by( baseball, year, id )
gpdt <- group_by( tbl_dt( baseball), year, id ) 
res  <- strip( filter_grouped_df( gps, g == max(g), year > 2000 ), order = TRUE )
res2 <- strip( filter( gps, g == max(g), year > 2000 )           , order = TRUE )
res3 <- strip( filter( gpdt, g == max(g), year > 2000 )          , order = TRUE )
# checking results are consistent
all.equal( strip(res), strip(res2) )
all.equal( strip(res), strip(res3) )

microbenchmark( 
    dplyrRcpp = filter_grouped_df( gps, g == max(g), year > 2000 ) , 
    dplyr = filter( gps, g == max(g), year > 2000 ), 
    dplyr_dt = filter( gpdt, g == max(g), year > 2000 ) 
)

