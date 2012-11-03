# dplyr

`dplyr` is the next iteration of plyr with the following goals:

* Move towards a grammar of data manipulation
* Support alternative data stores (data.table, sql, hive, ...)
* Be much much faster
* Focus on the most useful/common operations: ddply, ldply, dlply

One of the key ideas of `dplyr` is that it shouldn't matter how your data is stored, whether its in an SQL database, in a csv file, in memory as a data frame or a data table, you should interact with it in the exactly the same way. 

# Simpler helpers

To make it easier to get started with `dplyr` without have to learn a new way of thinking about data manipulation, there are four helper functions which work very similar to base R functions:

* `summarise_by`
* `subset_by`
* `mutate_by`
* `arrange_by`

(these are the equivalent of `qplot`).  Each of these has the same arguments:

* `.source`: a data frame or other data source
* `.group`: a grouping specification created with `group`
* `...`: named arguments specifying expressions in terms of the variables in the data source

For example, all the following expressions are equivalent:

    ddply(baseball, "id", summarise, g = mean(g))
   
     summarise_by(baseball, "id", g = mean(g))

    baseball_dt <- as.data.table(baseball)
    baseball_dt[, list(g = mean(g)), by = id]


