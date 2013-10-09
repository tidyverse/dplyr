# dplyr

`dplyr` is the next iteration of plyr with the following goals:

* Improved performance
* A more consistent interface focussed on tabular data 
  (e.g. ddply, ldply and dlply)
* Support for alternative data stores (data.table, sql, hive, ...)

One of the key ideas of `dplyr` is that it shouldn't matter how your data is stored. Regardless of whether your data in an SQL database, a data frame or a data table, you should interact with it in the exactly the same way. (That said, `dplyr` works with [tidy data](http://vita.had.co.nz/papers/tidy-data.html) so it can assume varaibles are always described in a consistent way.)

`dplyr` is not currently available on CRAN, but you can install it from github with:

```R
devtools::install_github("assertthat")
devtools::install_github("dplyr")
```

To get started, read the notes below, or try the intro vignette: `vignette("introduction", package = "dplyr")`

## `tbls`

The key object in dplyr is a _tbl_, a representation of a tabular data structure.
Currently `dplyr` supports:

* data frames
* [data tables](http://datatable.r-forge.r-project.org/)
* [SQLite](http://sqlite.org/)
* [PostgreSQL](http://www.postgresql.org/)/[Redshift](http://aws.amazon.com/redshift/)
* [MySQL](http://www.mysql.com/)/[MariaDB](https://mariadb.com/)
* [Bigquery](https://developers.google.com/bigquery/)

You can create them as follows:

```R
library(dplyr)
# Built in data frame
head(hflights)

# Coerce to data table
hflights_dt <- tbl_dt(hflights)

# Caches data in local SQLite db
hflights_db1 <- tbl(hflights_sqlite(), "hflights")

# Caches data in local postgres db
hflights_db2 <- tbl(hflights_postgres(), "hflights")
```

Each tbl also comes in a grouped variant which allows you to easily perform operations "by group":

```R
carriers_df  <- group_by(hflights, UniqueCarrier)
carriers_dt  <- group_by(hflights_dt, UniqueCarrier)
carriers_db1 <- group_by(hflights_db1, UniqueCarrier)
carriers_db2 <- group_by(hflights_db2, UniqueCarrier)
# This database has an index on the UniqueCarrier, which is a recommended
# minimum whenever you're doing group by queries
```

## Single table verbs

`dplyr` implements the following verbs useful for data manipulation:

* `select()`: focus on a subset of variables
* `filter()`: focus on a subset of rows
* `mutate()`: add new columns
* `summarise()`: reduce each group to a smaller number of summary statistics
* `arrange()`: re-order the rows

See `?manip` for more details.

They all work as similarly as possible across the range of data sources.  The main difference is performance:

```R
system.time(summarise(carriers_df, delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed 
#  0.010   0.002   0.012 
system.time(summarise(carriers_dt, delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed 
#  0.007   0.000   0.008 
system.time(summarise(collect(carriers_db1, delay = mean(ArrDelay))))
#   user  system elapsed 
#  0.402   0.058   0.465 
system.time(summarise(collect(carriers_db2, delay = mean(ArrDelay))))
#   user  system elapsed 
#  0.386   0.097   0.718 
```

The data frame and table methods are substantially faster than plyr. The database methods are slower, but can work with data that don't fit in memory.

```R
library(plyr)
system.time(ddply(hflights, "UniqueCarrier", summarise, 
  delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed 
#  0.527   0.078   0.604 
```

### `do()`

As well as the specialised operations described above, `dplyr` also provides the generic `do()` function which applies any R function to each group of the data.

Let's take the batting database from the built-in Lahman database. We'll group it by year, and then fit a model to explore the relationship between their number of at bats and runs:

```r
batting_db <- tbl(lahman(), "Batting")
batting_df <- collect(batting_db)
batting_dt <- tbl_dt(batting_df)

years_db <- group_by(batting_db, yearID)
years_df <- group_by(batting_df, yearID)
years_dt <- group_by(batting_dt, yearID)

system.time(do(years_db, failwith(NULL, lm), formula = R ~ AB))
system.time(do(years_df, failwith(NULL, lm), formula = R ~ AB))
system.time(do(years_dt, failwith(NULL, lm), formula = R ~ AB))
```

Note that if you are fitting lots of linear models, it's a good idea to use `biglm` because it creates model objects that are considerably smaller:

```R
library(biglm)
mod1 <- do(years_df, lm, formula = R ~ AB)
mod2 <- do(years_df, biglm, formula = R ~ AB)
print(object.size(mod1), unit = "MB")
print(object.size(mod2), unit = "MB")
```

### Binary verbs

As well as verbs that work on a single tbl, there are also a set of useful verbs that work with two tbls are a time: joins.  dplyr implements the four most useful joins from SQL:

* `inner_join(x, y)`: matching x + y
* `left_join(x, y)`: all x + matching y
* `semi_join(x, y)`: all x with match in y
* `anti_join(x, y)`: all x without match in y

Currently join variables must be the same in both the left-hand and right-hand sides.

### Other operations

All tbls also provide `head()` and `print()` methods. The default print method gives information about the data source and shows the first 10 rows and all the columns that will fit on one screen. 

## Plyr compatibility

Currently, it's not a good idea to have both dplyr and plyr loaded. This is just a short-term problem: in the long-term, I'll move the matching functions from plyr into dplyr, and add a dplyr dependency to plyr.
