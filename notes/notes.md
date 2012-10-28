# To do

Compute locally:

* if can't simplify sequence of actions in a way that can be done in-source
* if use functions not available in-source (e.g. median)

Figure out how to deal with joins. 

Need way of "compiling" R function to in db function: so user can see
exactly which functions get called.

Email Jeff Hammerbacher and Murray Stokely and ask if they know of people use hive/bigquery and plyr who could help me understand. Also ask for pointers about profiling and improving performance of queries.  Email Matt Dowle about data.table.

Need call parser for subset, mutate and summarise that identifies local variables vs. data variables (replacing local variables with their values).  Should replace known function names with customised equivalents that take inputs and return whatever the source needs. Also list which remote vars are used, and have syntax for favouring remote var over local (so could do myvar == remote(myvar))

Benchmarking tool: save variations in a list, and then provide call to list plus arguments to test. Ask author to keep default ordering. Add function to compare.  Would benchmarks be better written as markdown docs? (Since they incorporate r code and writing).  Would also be useful to be able to time each expression in a script and write out results as comments.

What common operations does data.table support?

Manipulatr/ggplot2 survey: how often do you use each plyr function: all the time, often, occassionally, never (hypothesis: ddply most freq, ldply/dply next, the rest hardly ever)

Need to support paging and some sort of system to determine if the results are too big for R.

# Docs

Hive, bigquery and sql are all very similar

https://developers.google.com/bigquery/docs/query-reference

https://cwiki.apache.org/confluence/display/Hive/LanguageManual+Select
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+GroupBy
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF
