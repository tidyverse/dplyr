<!-- README.md is generated from README.Rmd. Please edit that file -->
dplyr
=====

[![Build Status](https://travis-ci.org/hadley/dplyr.png?branch=master)](https://travis-ci.org/hadley/dplyr)

dplyr is the next iteration of plyr. It is focussed on tools for working with data frames (hence the `d` in its name). It has three main goals:

-   Identify the most important data manipulation tools needed for data analysis and make them easy to use in R.

-   Provide blazing fast performance for in-memory data by writing key pieces of code in [C++](http://www.rcpp.org/).

-   Use the same code interface to work with data no matter where it's stored, whether in a data frame, a data table or database.

You can install:

-   the latest released version from CRAN with

    ``` r
    install.packages("dplyr")
    ```

-   the latest development version from github with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("hadley/lazyeval")
    devtools::install_github("hadley/dplyr")
    ```

You'll probably also want to install the data packages used in most examples: `install.packages(c("nycflights13", "Lahman"))`.

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/hadley/dplyr/issues). For questions and other discussion, please use the [manipulatr mailing list](https://groups.google.com/group/manipulatr).

Learning dplyr
--------------

To get started, read the notes below, and then read the intro vignette: `vignette("introduction", package = "dplyr")`. To make the most of dplyr, I also recommend that you familiarise yourself with the principles of [tidy data](http://vita.had.co.nz/papers/tidy-data.html): this will help you get your data into a form that works well with dplyr, ggplot2 and R's many modelling functions.

If you need more, help I recommend the following (paid) resources:

-   [dplyr](https://www.datacamp.com/courses/dplyr) on datacamp, by Garrett Grolemund. Learn the basics of dplyr at your own pace in this interactive online course.

-   [Introduction to Data Science with R](http://shop.oreilly.com/product/0636920034834.do): How to Manipulate, Visualize, and Model Data with the R Language, by Garrett Grolemund. This O'Reilly video series will teach you the basics needed to be an effective analyst in R.

Key data structures
-------------------

The key object in dplyr is a *tbl*, a representation of a tabular data structure. Currently `dplyr` supports:

-   data frames
-   [data tables](https://github.com/Rdatatable/data.table/wiki)
-   [SQLite](http://sqlite.org/)
-   [PostgreSQL](http://www.postgresql.org/)/[Redshift](http://aws.amazon.com/redshift/)
-   [MySQL](http://www.mysql.com/)/[MariaDB](https://mariadb.com/)
-   [Bigquery](https://developers.google.com/bigquery/)
-   [MonetDB](http://www.monetdb.org/)
-   [Presto](https://prestodb.io/)
-   data cubes with arrays (partial implementation)

You can create them as follows:

``` r
library(dplyr) # for functions
library(nycflights13) # for data
flights
#> Source: local data frame [336,776 x 16]
#> 
#>     year month   day dep_time dep_delay arr_time arr_delay carrier tailnum
#>    (int) (int) (int)    (int)     (dbl)    (int)     (dbl)   (chr)   (chr)
#> 1   2013     1     1      517         2      830        11      UA  N14228
#> 2   2013     1     1      533         4      850        20      UA  N24211
#> 3   2013     1     1      542         2      923        33      AA  N619AA
#> 4   2013     1     1      544        -1     1004       -18      B6  N804JB
#> 5   2013     1     1      554        -6      812       -25      DL  N668DN
#> 6   2013     1     1      554        -4      740        12      UA  N39463
#> 7   2013     1     1      555        -5      913        19      B6  N516JB
#> 8   2013     1     1      557        -3      709       -14      EV  N829AS
#> 9   2013     1     1      557        -3      838        -8      B6  N593JB
#> 10  2013     1     1      558        -2      753         8      AA  N3ALAA
#> ..   ...   ...   ...      ...       ...      ...       ...     ...     ...
#> Variables not shown: flight (int), origin (chr), dest (chr), air_time
#>   (dbl), distance (dbl), hour (dbl), minute (dbl)

# Caches data in local SQLite db
flights_db1 <- tbl(nycflights13_sqlite(), "flights")

# Caches data in local postgres db
flights_db2 <- tbl(nycflights13_postgres(), "flights")
```

A tbl can be converted to a grouped variant that makes performing "by group" operations easy.:

``` r
carriers_df  <- flights %>% group_by(carrier)
carriers_db1 <- flights_db1 %>% group_by(carrier)
carriers_db2 <- flights_db2 %>% group_by(carrier)
```

Single table verbs
------------------

`dplyr` implements the following data manipulation verbs :

-   `select()`: selects a subset of columns or variables
-   `filter()`: selects a subset of rows or observations
-   `mutate()`: adds new columns or variables
-   `summarise()`: reduces a group(s) to a smaller number of values (e.g., summary statistics)
-   `arrange()`: re-orders rows or observations

They all work as similarly as possible across the range of data sources. The main difference is performance:

``` r
system.time(carriers_df %>% summarise(delay = mean(arr_delay)))
#>    user  system elapsed 
#>   0.036   0.001   0.037
system.time(carriers_db1 %>% summarise(delay = mean(arr_delay)) %>% collect())
#>    user  system elapsed 
#>   0.263   0.130   0.392
system.time(carriers_db2 %>% summarise(delay = mean(arr_delay)) %>% collect())
#>    user  system elapsed 
#>   0.016   0.001   0.151
```

Data frame methods are much much faster than their plyr equivalents. The database methods are slower, but can work with data that don't fit in memory.

``` r
system.time(plyr::ddply(flights, "carrier", plyr::summarise,
  delay = mean(arr_delay, na.rm = TRUE)))
#>    user  system elapsed 
#>   0.100   0.032   0.133
```

### `do()`

As well as the specialised operations described above, `dplyr` also provides a generic `do()` function that applies any R function to each group of the data.

Let's take the batting database from the built-in Lahman database. We'll group it by year, and then fit a model to explore the relationship between the number of at bats and runs:

``` r
by_year <- lahman_df() %>% 
  tbl("Batting") %>%
  group_by(yearID)
by_year %>% 
  do(mod = lm(R ~ AB, data = .))
#> Source: local data frame [143 x 2]
#> Groups: <by row>
#> 
#>    yearID     mod
#>     (int)   (chr)
#> 1    1871 <S3:lm>
#> 2    1872 <S3:lm>
#> 3    1873 <S3:lm>
#> 4    1874 <S3:lm>
#> 5    1875 <S3:lm>
#> 6    1876 <S3:lm>
#> 7    1877 <S3:lm>
#> 8    1878 <S3:lm>
#> 9    1879 <S3:lm>
#> 10   1880 <S3:lm>
#> ..    ...     ...
```

Note that if you are fitting lots of linear models, it's a good idea to use `biglm` because it creates model objects that are considerably smaller:

``` r
by_year %>% 
  do(mod = lm(R ~ AB, data = .)) %>%
  object.size() %>%
  print(unit = "MB")
#> 22.2 Mb

by_year %>% 
  do(mod = biglm::biglm(R ~ AB, data = .)) %>%
  object.size() %>%
  print(unit = "MB")
#> 0.8 Mb
```

Multiple table verbs
--------------------

Besides verbs that work on a single tbl, there are also a set of verbs that work with pairs of tbls: joins and set operations.

dplyr implements the four most useful SQL joins:

-   `inner_join(x, y)`: matching x + y
-   `left_join(x, y)`: all x + matching y
-   `semi_join(x, y)`: all x with match in y
-   `anti_join(x, y)`: all x without match in y

And provides methods for:

-   `intersect(x, y)`: all rows in both x and y
-   `union(x, y)`: rows in either x or y
-   `setdiff(x, y)`: rows in x, but not y

Plyr compatibility
------------------

You'll need to be a little careful if you load both plyr and dplyr at the same time. I'd recommend loading plyr before dplyr, so that the faster dplyr functions come first in the search path. By and large, any function provided by both dplyr and plyr works in a similar way, but dplyr functions tend to be faster and more general.

Related approaches
------------------

-   [Blaze](http://blaze.pydata.org)
-   [|Stat](http://oldwww.acm.org/perlman/stat/)
-   [Pig](http://dx.doi.org/10.1145/1376616.1376726)
