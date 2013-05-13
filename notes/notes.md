# To do

Figure out how to deal with joins. 

Email Jeff/Murray and ask if they know of people use hive/bigquery and plyr who could help me understand pain points. Ask for pointers about profiling and improving performance of queries.

Would also be useful to be able to time each expression in a script and write out results as comments.

# Docs

Hive, bigquery and sql are all very similar, but have slightly different in-source operations.  Need some dsl support to quickly add more.

https://developers.google.com/bigquery/docs/query-reference

https://cwiki.apache.org/confluence/display/Hive/LanguageManual+Select
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+GroupBy
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF

# RCpp

Could implement own out-of-memory csv, txt and fwf handlers. Would only pull data in as necessary.  Would need some way to specify column types quickly.  Maybe two-pass approach - first time it'd guess and then you could tweak.

mmap ?

# dplyr vs. plyr

Need to implement equivalents of `dlply` and `ddply`: but should dispatch on summary function and data type (suggesting S4 would be advantageous). Also need `ldply`, but that just involves setting up the attributes the same way as plyr.

For initial release need:

* methods for: 
  * filter (combine with and)
  * select (combine with c)
  * mutate (combine with c)
  * summarise (combined with c, only one of summarise/mutate)
  * arrange (combine with c)
  
  * do
  * count?
  * unique?

* for objects of type:
  * data frame
  * grouped data frame
  * data table
  * grouped data table
  * sqlite table
  * grouped sqlite table

Goal: work as lazily as possible (apart from `do()` and `count()`). Each manipulation just builds up and object that is eventually rendered by as.data.frame. (But even `as.data.frame()` needs to maintain the grouping information)

```R
players <- group(baseball, id)
players <- group_s(baseball, "id")
players <- group_q(baseball, quote(id))

cyear <- mutate(players, cyear = year - min(year) + 1)
do(cyear, lm, formula = g ~ cyear)

count(baseball) # same as nrow
count(baseball, "id")
# same as
count(group(players, id))

# Equivalent, but 2nd would be faster ?
# (unless inspected expression and hoisted to top-level)
count(subset(group(players, id), year > 2000))
count(group(subset(players, year > 2000), id))
```
