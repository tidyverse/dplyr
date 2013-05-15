# dplyr

`dplyr` is the next iteration of plyr with the following goals:

* Improved performance
* A more consistent interface focussed on tabular data 
  (e.g. ddply, ldply and dlply)
* Support for alternative data stores (data.table, sql, hive, ...)

One of the key ideas of `dplyr` is that it shouldn't matter how your data is stored, whether its in an SQL database, in a csv file, in memory as a data frame or a data table, you should interact with it in the exactly the same way. 

# Data sources

To get started with `dplyr`, you need a data source. Currently there is built-in support for data frames, data tables and SQLite databases.  You can create them as follows:

```R
library(dplyr)
data(baseball, package = "plyr")

baseball_df <- source_df(baseball)
baseball_dt <- source_dt(baseball)

db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
baseball_db <- source_sqlite(db_path, "baseball")
```

Each data source also comes in a grouped variant which allows you to easily perform operations "by group":

```R
players_df <- group_by(baseball_df, id)
players_dt <- group_by(baseball_dt, id)
players_db <- group_by(baseball_db, id)
```

# Operations

`dplyr` implements the following verbs useful for data manipulation:

* `select()`: focus on a subset of variables
* `filter()`: focus on a subset of rows
* `mutate()`: add new columns
* `summarise()`: reduce each group to a single row
* `arrange()`: re-order the rows

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
#  0.029   0.000   0.030 
```

And note that all methods are substantially faster than plyr:

```R
library(plyr)
system.time(ddply(baseball, "id", summarise, g = mean(g)))
#   user  system elapsed 
#  0.401   0.009   0.411 
```