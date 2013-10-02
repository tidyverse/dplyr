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

## `tbls`

The key object in dplyr is a _tbl_, a representation of a tabular data structure.
Currently `dplyr` supports data frames, data tables and SQLite databases. You can create them as follows:

```R
library(dplyr)
# Built in data frame
head(hflights)

# Coerce to data table
hflights_dt <- tbl_dt(hflights)

# Caches data in local SQLite db
hflights_db <- tbl(hflights_sqlite(), "hflights")

```

Each tbl also comes in a grouped variant which allows you to easily perform operations "by group":

```R
carriers_df <- group_by(hflights, UniqueCarrier)
carriers_dt <- group_by(hflights_dt, UniqueCarrier)
carriers_db <- group_by(hflights_db, UniqueCarrier)
# This database has an index on the player id, which is a recommended
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
system.time(summarise(carriers, delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed 
#  0.010   0.002   0.012 
system.time(summarise(carriers_dt, delay = mean(ArrDelay, na.rm = TRUE)))
#   user  system elapsed 
#  0.007   0.000   0.008 
system.time(summarise(collect(carriers_db, delay = mean(ArrDelay))))
#   user  system elapsed 
#  0.040   0.005   0.052 
# quiet a bit faster on "warm" database
#   user  system elapsed 
#  0.035   0.000   0.035 
```

All methods are substantially faster than plyr:

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

You can also join data sources: this is currently only supported for SQL, but data frame and data table wrappers will be added in the near future.

* inner join
* left join
* semi join
* anti join

Currently joins variables must be the same in both the left-hand and right-hand sides.

### Compound

Sqlite tbls also support the compound operator, which allows you to union all tbls together.

### Other operations

All tbls also provide `head()`, `tail()` and `print()` methods. The default print method gives information about the data source and shows the first 10 rows and all the columns that will fit on one screen. 

## Plyr compatibility

Currently, it's not a good idea to have both dplyr and plyr loaded. This is just a short-term problem: in the long-term, I'll move the matching functions from plyr into dplyr, and add a dplyr dependency to plyr.
