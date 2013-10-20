require(methods)
require(dplyr)
require(microbenchmark)

sort_ <- dplyr:::sort_

data("baseball", package = "plyr")
microbenchmark( 
    "dplyr::sort_" = sort_( baseball ) , 
    base = baseball[ do.call( order, baseball ) , , drop = FALSE]
)

