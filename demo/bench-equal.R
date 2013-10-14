require(dplyr, quietly = TRUE)
require(methods, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

scramble <- function(x){
    x[ sample(1:nrow(x)), sample(1:ncol(x)) ]
}

hflights2 <- scramble(hflights)
equal_ <- dplyr:::equal_ 
equal_( hflights, hflights2 )
equal_( hflights, iris )
equal_( hflights, filter( hflights, Dest == "DFW" ) )
equal_( hflights, mutate( hflights, PM = ifelse( DepTime > 1200, TRUE, FALSE) ) )

