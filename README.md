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
data(baseball, package = "plyr")

baseball_df <- tbl_df(baseball)
baseball_dt <- tbl_dt(baseball)

db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_db <- tbl_sqlite(db_path, "baseball")
```

Each tbl also comes in a grouped variant which allows you to easily perform operations "by group":

```R
players_df <- group_by(baseball_df, id)
players_dt <- group_by(baseball_dt, id)
players_db <- group_by(baseball_db, id)
# This database has an index on the player id, which is a recommended
# minimum whenever you're doing group by queries
```

## Operations

`dplyr` implements the following verbs useful for data manipulation:

* `select()`: focus on a subset of variables
* `filter()`: focus on a subset of rows
* `mutate()`: add new columns
* `summarise()`: reduce each group to a single row
* `arrange()`: re-order the rows

See `?manip` for more details.

They all work as similarly as possible across the range of data sources.  The main difference is performance:

```R
system.time(summarise(players_df, g = mean(g)))
#   user  system elapsed 
#  0.034   0.000   0.034
system.time(summarise(players_dt, g = mean(g)))
#   user  system elapsed 
#  0.007   0.000   0.007 
system.time(summarise(players_db, g = mean(g)))
#   user  system elapsed 
#  0.029   0.000   0.019 
```

And note that all methods are substantially faster than plyr:

```R
library(plyr)
system.time(ddply(baseball, "id", summarise, g = mean(g)))
#   user  system elapsed 
#  0.401   0.009   0.411 
```

### `do()`

As well as the specialised operations described above, `dplyr` also provides the generic `do()` function which applies any R function to each group of the data.

For example, we could use `do()` to fit a linear model to each player in the database:

```R
system.time(do(players_df, failwith(NULL, lm), formula = r ~ ab))
system.time(do(players_dt, failwith(NULL, lm), formula = r ~ ab))
system.time(do(players_db, failwith(NULL, lm), formula = r ~ ab))
```

Note that if you are fitting lots of linear models, it's a good idea to use `biglm` because it creates model objects that are considerably smaller:

```R
library(biglm)
mod1 <- do(players_df, lm, formula = r ~ ab)
mod2 <- do(players_df, biglm, formula = r ~ ab)
print(object.size(mod1), unit = "MB")
print(object.size(mod2), unit = "MB")
```

### Other operations

All tbls also provide `head()`, `tail()` and `print()` methods. The default print method gives information about the data source and shows the first 10 rows and all the columns that will fit on one screen. 

## Plyr compatibility

Currently, it's not a good idea to have both dplyr and plyr loaded. This is just a short-term problem: in the long-term, I'll move the matching functions from plyr into dplyr, and add a dplyr dependency to plyr.
