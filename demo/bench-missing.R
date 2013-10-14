require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

scramble <- function(x){
    x[ sample(1:nrow(x)), sample(1:ncol(x)) ]
}
arrange_ <- dplyr:::arrange_

d <- data.frame( 
    a = rep( c(NA,1,2,3), each = 4 ), 
    b = rep( c(0L,NA,1L,2L), 4 ), 
    c = local( { z <- LETTERS[6:21]; z[1:4] <- NA; z } ), 
    d = rep( c(T,NA,F,T), each = 4 ),
    stringsAsFactors = FALSE
)
na_last <- function(x){
    print(x)
    all( is.na( tail(x,4) ) )
    message("")
}

d

options( width = 150 )
message("numeric")
na_last( arrange_(d, a)      [,"a"] )
na_last( arrange_(d, desc(a))[,"a"] )

message("integer")
na_last( arrange_(d, b)      [,"b"] )   
na_last( arrange_(d, desc(b))[,"b"] )

message("character")
na_last( arrange_(d, c)      [,"c"] )
na_last( arrange_(d, desc(c))[,"c"] )

message("logical")
na_last( arrange_(d, d)      [,"d"] )
na_last( arrange_(d, desc(d))[,"d"] )

