# To do

Compute locally:

* if can't simplify sequence of actions in a way that can be done in-source
* if use functions not available in-source (e.g. median)

Figure out how to deal with joins. 

Email Jeff/Murray and ask if they know of people use hive/bigquery and plyr who could help me understand pain points. Also ask for pointers about profiling and improving performance of queries.

Would also be useful to be able to time each expression in a script and write out results as comments.

Need to support paging and some sort of system to determine if the results are too big for R.

Need to continue to support parallel backends.

# Docs

Hive, bigquery and sql are all very similar

https://developers.google.com/bigquery/docs/query-reference

https://cwiki.apache.org/confluence/display/Hive/LanguageManual+Select
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+GroupBy
https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF

