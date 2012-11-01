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