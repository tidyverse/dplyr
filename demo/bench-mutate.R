require(methods, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

strip <- function(x, order = FALSE) {
  x <- as.data.frame(x)
  if (order) x <- x[do.call("order", x), , drop = FALSE]
  rownames(x) <- NULL
  attr( x, "vars") <- NULL
  x
}

mutate_ <- dplyr:::mutate_

hflights_dt <- tbl_dt( hflights )
hflights_df <- tbl_df( hflights )

res_dplyr <- mutate(hflights_df, gained = ArrDelay - DepDelay)
res_dplyrRcpp <- mutate_(hflights, gained = ArrDelay - DepDelay)
all.equal( strip(res_dplyr), strip(res_dplyrRcpp) )

microbenchmark( 
    dplyr     = mutate(hflights_df, gained = ArrDelay - DepDelay),
    dplyr_dt  = mutate(hflights_dt, gained = ArrDelay - DepDelay),
    dplyrRcpp = mutate_(hflights, gained = ArrDelay - DepDelay)
)


by_dest     <- group_by(hflights, Dest)
by_dest_dt  <- group_by(tbl_dt(hflights), Dest)

dplyrRcpp = mutate_( by_dest   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   
dplyr     = mutate ( by_dest   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   
dplyr_dt  = mutate ( by_dest_dt, arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )

# checking results are consistent
all.equal( strip(dplyr), strip(dplyrRcpp) )
all.equal( strip(dplyr, order = TRUE), strip(dplyr_dt, order = TRUE) )

# q("no")

microbenchmark( 
    dplyrRcpp = mutate_( by_dest   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   , 
    dplyr     = mutate ( by_dest   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   , 
    dplyr_dt  = mutate ( by_dest_dt, arr_z = scale(ArrDelay), dep_z = scale(DepDelay) ) 
)

