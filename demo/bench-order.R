require(methods)
require(dplyr)
require(microbenchmark)

order_ <- dplyr:::order_

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)

res1 <- order_( x, desc(y), z )
res2 <- order( x, desc(y), z )
           
stopifnot(identical( res1, res2))

x <- rnorm(1e5)
y <- rnorm(1e5)
z <- rnorm(1e5)

microbenchmark( 
    cpp  = order_( x, desc(y), z ), 
    base = order( x, desc(y), z )
)

data("baseball", package = "plyr")
microbenchmark( 
    cpp  = order_( id, year, data = baseball) , 
    base = with( baseball, order( id, year ) )
)

