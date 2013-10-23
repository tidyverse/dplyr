require(dplyr)
require(microbenchmark) 

x <- data.frame( x = c(1:2, 5L ), y = letters[c(1:2, 5)], stringsAsFactors = FALSE ) 
y <- data.frame( x = c(2:1, 4L ), y = letters[c(2:1, 4)], stringsAsFactors = FALSE ) 
x <- rbind( x, x )
y <- rbind(y,y)

message( "union" )
dplyr:::union_data_frame( x, y )

message( "intersect" )
dplyr:::intersect_data_frame( x, y )

message( "setdiff" )
dplyr:::setdiff_data_frame( x, y )

message( "match" )
dplyr:::match_data_frame( x, y )

