<!-- README.md is generated from README.Rmd. Please edit that file -->
dplyr
=====

[![Build Status](https://travis-ci.org/hadley/dplyr.png?branch=master)](https://travis-ci.org/hadley/dplyr)
[![Coverage Status](https://img.shields.io/coveralls/hadley/dplyr.svg)](https://coveralls.io/r/hadley/dplyr?branch=master)

dplyr is the next iteration of plyr, focussed on tools for working with data frames (hence the `d` in the name). It has three main goals:

-   Identify the most important data manipulation tools needed for data analysis and make them easy to use from R.

-   Provide blazing fast performance for in-memory data by writing key pieces in [C++](http://www.rcpp.org/).

-   Use the same interface to work with data no matter where it's stored, whether in a data frame, a data table or database.

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

To get started, read the notes below, then read the intro vignette: `vignette("introduction", package = "dplyr")`. To make the most of dplyr, I also recommend that you familiarise yourself with the principles of [tidy data](http://vita.had.co.nz/papers/tidy-data.html): this will help you get your data into a form that works well with dplyr, ggplot2 and R's many modelling functions.

If you need more, help I recommend the following (paid) resources:

-   [dplyr](https://www.datacamp.com/courses/dplyr) on datacamp, by Garrett Grolemund. Learn the basics of dplyr at your own pace in this interactive online course.

-   [Introduction to Data Science with R](http://shop.oreilly.com/product/0636920034834.do): How to Manipulate, Visualize, and Model Data with the R Language, by Garrett Grolemund. This O'Reilly video series will teach you the basics to be an effective analyst in R.

Key data structures
-------------------

The key object in dplyr is a *tbl*, a representation of a tabular data structure. Currently `dplyr` supports:

-   data frames
-   [data tables](http://datatable.r-forge.r-project.org/)
-   [SQLite](http://sqlite.org/)
-   [PostgreSQL](http://www.postgresql.org/)/[Redshift](http://aws.amazon.com/redshift/)
-   [MySQL](http://www.mysql.com/)/[MariaDB](https://mariadb.com/)
-   [Bigquery](https://developers.google.com/bigquery/)
-   [MonetDB](http://www.monetdb.org/) (via [MonetDB.R](http://monetr.r-forge.r-project.org/))
-   data cubes with arrays (partial implementation)

You can create them as follows:

``` r
library(dplyr) # for functions
library(nycflights13) # for data
flights

# Caches data in local SQLite db
flights_db1 <- tbl(nycflights13_sqlite(), "flights")

# Caches data in local postgres db
flights_db2 <- tbl(nycflights13_postgres(), "flights")
```

Each tbl also comes in a grouped variant which allows you to easily perform operations "by group":

``` r
carriers_df  <- group_by(flights, carrier)
carriers_db1 <- group_by(flights_db1, carrier)
carriers_db2 <- group_by(flights_db2, carrier)
```

Single table verbs
------------------

`dplyr` implements the following verbs useful for data manipulation:

-   `select()`: focus on a subset of variables
-   `filter()`: focus on a subset of rows
-   `mutate()`: add new columns
-   `summarise()`: reduce each group to a smaller number of summary statistics
-   `arrange()`: re-order the rows

They all work as similarly as possible across the range of data sources. The main difference is performance:

``` r
system.time(summarise(carriers_df, delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed
#  0.010   0.002   0.012
system.time(summarise(collect(carriers_db1, delay = mean(ArrDelay))))
#   user  system elapsed
#  0.402   0.058   0.465
system.time(summarise(collect(carriers_db2, delay = mean(ArrDelay))))
#   user  system elapsed
#  0.386   0.097   0.718
```

The data frame methods are all at least an order of magnitude faster than the plyr equivalent. The database methods are slower, but can work with data that don't fit in memory.

``` r
library(plyr)
system.time(ddply(hflights, "UniqueCarrier", summarise,
  delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed
#  0.527   0.078   0.604
```

### `do()`

As well as the specialised operations described above, `dplyr` also provides the generic `do()` function which applies any R function to each group of the data.

Let's take the batting database from the built-in Lahman database. We'll group it by year, and then fit a model to explore the relationship between their number of at bats and runs:

``` r
batting_db <- tbl(lahman_sqlite(), "Batting")
batting_df <- collect(batting_db)

years_db <- group_by(batting_db, yearID)
years_df <- group_by(batting_df, yearID)

system.time(do(years_db, failwith(NULL, lm), formula = R ~ AB))
system.time(do(years_df, failwith(NULL, lm), formula = R ~ AB))
```

Note that if you are fitting lots of linear models, it's a good idea to use `biglm` because it creates model objects that are considerably smaller:

``` r
library(biglm)
mod1 <- do(years_df, lm, formula = R ~ AB)
mod2 <- do(years_df, biglm, formula = R ~ AB)
print(object.size(mod1), unit = "MB")
print(object.size(mod2), unit = "MB")
```

Multiple table verbs
--------------------

As well as verbs that work on a single tbl, there are also a set of useful verbs that work with two tbls at a time: joins and set operations.

dplyr implements the four most useful joins from SQL:

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

You'll need to be a little careful if you load both plyr and dplyr at the same time. I'd recommend loading plyr first, then dplyr, so that the faster dplyr functions come first in the search path. By and large, any function provided by both dplyr and plyr works in a similar way, although dplyr functions tend to be faster and more general.

Related approaches
------------------

-   [Blaze](http://blaze.pydata.org)
-   [|Stat](http://hcibib.org/perlman/stat/introman.html)
-   [Pig](http://infolab.stanford.edu/~usriv/papers/pig-latin.pdf)
