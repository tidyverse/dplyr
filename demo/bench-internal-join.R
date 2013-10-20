require(dplyr)
require(microbenchmark) 

x <- data.frame( x = c(1:2, 5L ), y = letters[c(1:2, 5)], a = c(10, 20, 30), stringsAsFactors = FALSE ) 
y <- data.frame( x = c(2:1, 4L ), y = letters[c(2:1, 4)], z = c(1.5, 2.5, 3.5), stringsAsFactors = FALSE ) 
x <- rbind( x, x )
y <- rbind(y,y)

equal_ <- dplyr:::equal_

data("Batting", package = "Lahman")
data("Master", package = "Lahman")

batting_cpp <- tbl_cpp(Batting)
person_cpp  <- tbl_cpp(Master)

batting_df  <- tbl_df(Batting)
person_df   <- tbl_df(Master)

batting_dt  <- tbl_dt(Batting)
person_dt   <- tbl_dt(Master)
                   
stopifnot( equal_( semi_join( batting_df, person_df ), semi_join( batting_cpp, person_cpp ) ) )
stopifnot( equal_( anti_join( batting_df, person_df ), anti_join( batting_cpp, person_cpp ) ) )

# fails now because there are Date columns
# stopifnot( equal_( left_join( batting_df, person_df ), left_join( batting_cpp, person_cpp ) ) )
# stopifnot( equal_( inner_join( batting_df, person_df ), inner_join( batting_cpp, person_cpp ) ) )
  
microbenchmark( 
    cpp = semi_join( batting_cpp, person_cpp ), 
    df  = semi_join( batting_df, person_df ), 
    dt  = semi_join( batting_dt, person_dt )
)
microbenchmark( 
    cpp = anti_join( batting_cpp, person_cpp ), 
    df  = anti_join( batting_df, person_df ), 
    dt  = anti_join( batting_dt, person_dt )
)
# microbenchmark( 
#     cpp = inner_join( batting_cpp, person_cpp ), 
#     df  = inner_join( batting_df, person_df ), 
#     dt  = inner_join( batting_dt, person_dt )
# )
# microbenchmark( 
#     cpp = left_join( batting_cpp, person_cpp ), 
#     df  = left_join( batting_df, person_df ), 
#     dt  = left_join( batting_dt, person_dt )
# )

