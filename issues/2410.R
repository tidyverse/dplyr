devtools::load_all()
#>
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#>
#>     filter, lag
#> The following objects are masked from 'package:base':
#>
#>     intersect, setdiff, setequal, union
my_db <- src_sqlite("my_db.sqlite3", create = T)
foo <- data.frame(a=1:3, c=4:6)
bar <- data.frame(b=1:3, d=7:9)
foo <- copy_to(my_db, foo)
bar <- copy_to(my_db, bar)
foo
#> Source:   query [?? x 2]
#> Database: sqlite 3.16.2 [my_db.sqlite3]
#>
#>       a     c
#>   <int> <int>
#> 1     1     4
#> 2     2     5
#> 3     3     6
bar
#> Source:   query [?? x 2]
#> Database: sqlite 3.16.2 [my_db.sqlite3]
#>
#>       b     d
#>   <int> <int>
#> 1     1     7
#> 2     2     8
#> 3     3     9
foobar <- inner_join(foo, bar, by=c("a"="b"))
colnames(foobar)
#> [1] "a" "c" "d"
print(foobar)
#> Source:   query [?? x 3]
#> Database: sqlite 3.16.2 [my_db.sqlite3]
#>
#>       a     c     b     d
#>   <int> <int> <int> <int>
#> 1     1     4     1     7
#> 2     2     5     2     8
#> 3     3     6     3     9
stopifnot(identical(tbl_vars(foobar), names(collect(foobar))))
