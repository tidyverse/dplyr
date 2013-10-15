require(methods)
require(dplyr)
require(microbenchmark)
require(data.table)

data("baseball", package = "plyr")
arrange_ <- dplyr:::arrange_
equal_   <- dplyr:::equal_

baseball_dt <- data.table(baseball)

res_internal <- arrange_( baseball  , id, year )
res_dplyr    <- arrange( baseball  , id, year )
stopifnot( equal_( res_internal, res_dplyr ) )

microbenchmark( 
    internal  = arrange_( baseball  , id, year ) , 
    dplyr     = arrange( baseball   , id, year ),
    dplyr_dt  = arrange( baseball_dt, id, year ),
    base = baseball[ with( baseball, order( id, year ) ), , drop = FALSE]
)

