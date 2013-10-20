require(methods, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(microbenchmark, quietly = TRUE)

hflights_dt  <- tbl_dt( hflights )
hflights_df  <- tbl_df( hflights )
hflights_cpp <- tbl_cpp( hflights )

by_dest_df   <- group_by(hflights_df , Dest)
by_dest_dt   <- group_by(hflights_dt , Dest)
by_dest_cpp  <- group_by(hflights_cpp, Dest)

stopifnot( dplyr:::all_equal_( 
    mutate(hflights_df, gained = ArrDelay - DepDelay), 
    mutate(hflights_dt, gained = ArrDelay - DepDelay),
    mutate(hflights_cpp, gained = ArrDelay - DepDelay)
) )

microbenchmark( 
    df  = mutate(hflights_df , gained = ArrDelay - DepDelay),
    dt  = mutate(hflights_dt , gained = ArrDelay - DepDelay),
    cpp = mutate(hflights_cpp, gained = ArrDelay - DepDelay)
)

stopifnot( dplyr:::all_equal_( 
    mutate( by_dest_df   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) ), 
    mutate( by_dest_dt   , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) ),
    mutate( by_dest_cpp  , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )
) )

microbenchmark( 
    cpp = mutate( by_dest_cpp , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   , 
    df  = mutate( by_dest_df  , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) )   , 
    dt  = mutate( by_dest_dt  , arr_z = scale(ArrDelay), dep_z = scale(DepDelay) ) 
)

