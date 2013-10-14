require(methods)
require(dplyr)
require(microbenchmark)

data("baseball", package = "plyr")
arrange_ <- dplyr:::arrange_
microbenchmark( 
    dplyrRcpp = arrange_( baseball, id, year) , 
    dplyr     = arrange( baseball, id, year ),
    base = baseball[ with( baseball, order( id, year ) ), , drop = FALSE]
)

