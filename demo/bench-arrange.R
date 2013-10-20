require(methods)
require(dplyr)
require(microbenchmark)
require(data.table)

data("baseball", package = "plyr")
all_equal_   <- dplyr:::all_equal_

baseball_dt  <- tbl_dt(baseball)
baseball_df  <- tbl_df(baseball)
baseball_cpp <- tbl_cpp(baseball)

res_cpp   <- arrange( baseball_cpp , id, year )
res_df    <- arrange( baseball_df  , id, year )
res_dt    <- arrange( baseball_dt  , id, year )

stopifnot( all_equal_( res_cpp, res_df, res_dt ) )

microbenchmark( 
    cpp   = arrange( baseball_cpp, id, year ), 
    df    = arrange( baseball_df , id, year ),
    dt    = arrange( baseball_dt , id, year ),
    base = baseball[ with( baseball, order( id, year ) ), , drop = FALSE]
)

