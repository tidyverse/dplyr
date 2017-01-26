library(tidyverse, warn.conflicts = FALSE)

# Insert some NAs, convert doubles to integers
to_fix <- as.matrix(mtcars[1:3])
diag(to_fix) <- NA
to_fix <- to_fix %>% as.data.frame() %>% mutate_all(as.integer)

replacements <- mtcars[1:3]

str(to_fix)   # Integers
#> 'data.frame':    32 obs. of  3 variables:
#>  $ mpg : int  NA 21 22 21 18 18 14 24 22 19 ...
#>  $ cyl : int  6 NA 4 6 8 6 8 4 4 6 ...
#>  $ disp: int  160 160 NA 258 360 225 360 146 140 167 ...
str(replacements)    # Doubles
#> 'data.frame':    32 obs. of  3 variables:
#>  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#>  $ disp: num  160 160 108 258 360 ...

coalesce(to_fix$mpg, replacements$mpg)    # Type error
#> Error: Vector 1 has type 'double' not 'integer'

coalesce(to_fix, replacements) %>% str()    # Works??? Coerces all back to double
#> 'data.frame':    32 obs. of  3 variables:
#>  $ mpg : num  21 21 22 21 18 18 14 24 22 19 ...
#>  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
#>  $ disp: num  160 160 108 258 360 225 360 146 140 167 ...

coalesce(as_data_frame(to_fix), replacements)    # Fails if one is tibble
#> Error: Vector 1 has class data.frame not tbl_df/tbl/data.frame

coalesce(as_data_frame(to_fix), as_data_frame(replacements))    # Fails differently if both tibble
#> Error: Unsupported use of matrix or array for column indexing

coalesce(as.matrix(to_fix), as.matrix(replacements))    # Matrices with different types fail
#> Error: Vector 1 has type 'double' not 'integer'

coalesce(to_fix %>% mutate_all(as.numeric) %>% as.matrix(),
         as.matrix(replacements)) %>% str()    # Matrices with same types work!
#>  num [1:32, 1:3] 21 21 22 21 18 18 14 24 22 19 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:3] "mpg" "cyl" "disp"

# With list column, coalesces non-list columns, makes everything list column
coalesce(as.data.frame(nest(to_fix, -cyl)),
         as.data.frame(nest(replacements, -cyl)))  %>% str()
#> 'data.frame':    4 obs. of  2 variables:
#>  $ cyl :List of 4
#>   ..$ : int 6
#>   ..$ : num 4
#>   ..$ : int 4
#>   ..$ : int 8
#>  $ data:List of 4
#>   ..$ :Classes 'tbl_df', 'tbl' and 'data.frame': 6 obs. of  2 variables:
#>   .. ..$ mpg : int  NA 21 18 19 17 19
#>   .. ..$ disp: int  160 258 225 167 167 145
#>   ..$ :Classes 'tbl_df', 'tbl' and 'data.frame': 1 obs. of  2 variables:
#>   .. ..$ mpg : int 21
#>   .. ..$ disp: int 160
#>   ..$ :Classes 'tbl_df', 'tbl' and 'data.frame': 11 obs. of  2 variables:
#>   .. ..$ mpg : int  22 24 22 32 30 33 21 27 26 30 ...
#>   .. ..$ disp: int  NA 146 140 78 75 71 120 79 120 95 ...
#>   ..$ :Classes 'tbl_df', 'tbl' and 'data.frame': 14 obs. of  2 variables:
#>   .. ..$ mpg : int  18 14 16 17 15 10 10 14 15 15 ...
#>   .. ..$ disp: int  360 360 275 275 275 472 460 440 318 304 ...

# Works on first level of lists with one number per element
coalesce(list(1, NA, 3.2, list(NA)), list(1L, 2L, 3L, list(4L))) %>% str()
#> List of 4
#>  $ : num 1
#>  $ : int 2
#>  $ : num 3.2
#>  $ :List of 1
#>   ..$ : logi NA

# With more complicated lists, returns x, but doesn't coalesce anything.
# Not sure why coalesce(to_fix, replacements) works but this doesn't.
coalesce(unclass(to_fix), unclass(replacements)) %>% str()
#> List of 3
#>  $ mpg : int [1:32] NA 21 22 21 18 18 14 24 22 19 ...
#>  $ cyl : int [1:32] 6 NA 4 6 8 6 8 4 4 6 ...
#>  $ disp: int [1:32] 160 160 NA 258 360 225 360 146 140 167 ...
#>  - attr(*, "row.names")= int [1:32] 1 2 3 4 5 6 7 8 9 10 ...
