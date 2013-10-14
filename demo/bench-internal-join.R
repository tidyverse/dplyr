require(dplyr)
require(microbenchmark) 

x <- data.frame( x = c(1:2, 5L ), y = letters[c(1:2, 5)], a = c(10, 20, 30), stringsAsFactors = FALSE ) 
y <- data.frame( x = c(2:1, 4L ), y = letters[c(2:1, 4)], z = c(1.5, 2.5, 3.5), stringsAsFactors = FALSE ) 
# x <- rbind( x, x )
# y <- rbind(y,y)

equal_ <- dplyr:::equal_
semi_join_impl <- dplyr:::semi_join_impl
anti_join_impl <- dplyr:::anti_join_impl
inner_join_impl <- dplyr:::inner_join_impl
left_join_impl <- dplyr:::left_join_impl

semi_join(x, y)
semi_join_impl(x, y )
anti_join(x, y)
anti_join_impl( x, y )

inner_join(x,y)
inner_join_impl(x,y)

left_join(x,y)
left_join_impl(x,y)

data("Batting", package = "Lahman")
data("Master", package = "Lahman")

batting_df <- tbl_df(Batting)
person_df <- tbl_df(Master)

dplyr <- semi_join( batting_df, person_df )
internal <- semi_join_impl( batting_df, person_df )
equal_( dplyr, internal )

dplyr <- anti_join( batting_df, person_df )
internal <- anti_join_impl( batting_df, person_df )
equal_( dplyr, internal )

dplyr <- inner_join(batting_df, person_df)
internal <- inner_join_impl(batting_df, person_df)
equal_( dplyr, internal )

dplyr <- left_join(batting_df, person_df)
internal <- left_join_impl(batting_df, person_df)
equal_( dplyr, internal )

microbenchmark( 
    dplyr = semi_join( batting_df, person_df ), 
    internal = semi_join_impl( batting_df, person_df )
)

microbenchmark( 
    dplyr = anti_join( batting_df, person_df ), 
    internal = anti_join_impl( batting_df, person_df )
)

microbenchmark( 
    dplyr = inner_join(batting_df, person_df), 
    internal = inner_join_impl(batting_df, person_df)
)
microbenchmark( 
    dplyr = left_join(batting_df, person_df), 
    internal = left_join_impl(batting_df, person_df)
)

