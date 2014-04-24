# dplyr 0.1.3.0.99

## Piping

dplyr now imports `%>%` from magrittr (#330). I recommend that you use this instead of `%.%` because it is easier to type (since you can hold down the shift key) and is more flexible. With you `%>%`, you can control which argument on the RHS recieves the LHS by using the pronoun `.`. This makes 
`%>%` more useful with base R functions because they don't always take the data frame as the first argument. For example you could pipe `mtcars` to `xtabs()` with:

    mtcars %>% xtabs( ~ cyl + vs, data = .)
    
Thanks to @smbache for the excellent margrittr package. dplyr only provides `%>%` from magrittr, but it contains many other useful functions. To use them, load `magrittr` explicitly: `library(magrittr)`. For more details, see `vignette("magrittr")`.

`%.%` will be deprecated in a future version of dplyr, but it won't happen for a while. I've also deprecated `chain()` to encourage a single style of dplyr usage: please use `%>%` instead.

## Do

`do()` has been completely overhauled. There are now two ways to use it, either with multiple named arguments or a single unnamed arguments. `group_by()` + `do()` is equivalent to `plyr::dlply`, except it always returns a data frame.

If you use named arguments, each argument becomes a list-variable in the output. A list-variable can contain any arbitrary R object so it's particularly well suited for storing models.

    library(dplyr)
    models <- mtcars %>% group_by(cyl) %>% do(lm = lm(mpg ~ wt, data = .))
    models %>% summarise(rsq = summary(lm)$r.squared)
    
If you use an unnamed argument, the result should be a data frame. This allows you to apply arbitrary functions to each group.

    mtcars %>% group_by(cyl) %>% do(head(., 1))

Note the use of the `.` pronoun to refer to the data in the current group.

`do()` also has an automatic progress bar. It appears if the computation takes longer than 5 seconds and lets you know (approximately) how much longer the job will take to complete.

## New verbs

dplyr 0.2 adds three new verbs:

* `glimpse()` makes it possible to see all the columns in a tbl,
  displaying as much data for each variable as can be fit on a single line.

* `sample_n()` randomly samples a fixed number of rows from a tbl;
  `sample_frac()` randomly samples a fixed fraction of rows. Only works
  for local data frames and data tables (#202).

* `summarise_each()` and `mutate_each()` make it easy to apply one or more
  functions to multiple columns in a tbl (#178).

## Minor improvements

* `as.tbl_cube()` gains a method for matrices (#359, @paulstaab)

* `compute()` gains `temporary` argument so you can control whether the
  results are temporary or permanent (#382, @cpsievert)

* `group_by()` now defaults to `add = FALSE` so that it sets the grouping
  variables rather than adding to the existing list. I think this is how
  most people expected `group_by` to work anyway, so it's unlikely to
  cause problems (#385).

* Support for [MonetDB](http://www.monetdb.org) tables with `src_monetdb()` 
  (#8, thanks to @hannesmuehleisen).

* New vignettes: 
  
    * `memory` vignette which discusses how dplyr minimises memory usage
      for local data frames (#198).
      
    *  `new-sql-backend` vignette which discusses how to add a new
       SQL backend/source to dplyr.

* `changes()` output more clearly distinguishes which columns were added or
  deleted.

* `explain()` is now generic.

* dplyr is more careful when setting the keys of data tables, so it never
  accidentally modifies an object that it doesn't own. It also avoids
  unnecessary key setting which negatively affected performance.
  (#193, #255).

* `print()` methods for `tbl_df`, `tbl_dt` and `tbl_sql` gain `n` argument to
  control the number of rows printed (#362). They also works better when you have 
  columns containing lists of complex objects. 

* `row_number()` can be called without arguments, in which case it returns
  the same as `1:n()` (#303).

* `"comment"` attribute is allowed (white listed) as well as names (#346).

* hybrid versions of `min`, `max`, `mean`, `var`, `sd` and `sum` 
  handle the `na.rm` argument (#168). This should yield substantial
  performance improvements for those functions.

* Special case for call to `arrange()` on a grouped data frame with no arguments. (#369)  
  
## Bug fixes

* Code adapted to Rcpp > 0.11.1

* internal `DataDots` class protects against missing variables in verbs (#314),
  including the case where `...` is missing. (#338)

* `all.equal.data.frame` from base is no longer bypassed. we now have 
  `all.equal.tbl_df` and `all.equal.tbl_dt` methods (#332). 
  
* `arrange()` correctly handles NA in numeric vectors (#331) and 0 row 
  data frames (#289).

* `copy_to.src_mysql()` now works on windows (#323)

* `*_join()` doesn't reorder column names (#324).

* `rbind_all()` is stricter and only accepts list of data frames (#288)

* `rbind_*` propagates time zone information for `POSIXct` columns (#298).

* `rbind_*` is less strict about type promotion. The numeric `Collecter` allows
  collection of integer and logical vectors. The integer `Collecter` also collects
  logical values (#321).

* internal `sum` correctly handles integer (under/over)flow (#308).

* `summarise()` checks consistency of outputs (#300) and drops `names` 
  attribute of output columns (#357). 

* join functions throw error instead of crashing when there are no common
  variables between the data frames, and also give a better error message when
  only one data frame has a by variable (#371). 

* `top_n()` returns `n` rows instead of `n - 1` (@leondutoit, #367).

* SQL translation always evaluates subsetting operators (`$`, `[`, `[[`) 
  locally. (#318).

* `select()` now renames variables in remote sql tbls (#317) and  
  implicitly adds grouping variables (#170).
  
* internal `grouped_df_impl` function errors if there are no variables to group by (#398). 

* `n_distinct` did not treat NA correctly in the numeric case #384. 

* Some compiler warnings triggered by -Wall or -pedantic have been eliminated. 

* `group_by` only creates one group for NA (#401). 

* Hybrid evaluator did not evaluate expression in correct environment (#403). 

# dplyr 0.1.3

## Bug fixes

* `select()` actually renames columns in a data table (#284).

* `rbind_all()` and `rbind_list()` now handle missing values in factors (#279).

* SQL joins now work better if names duplicated in both x and y tables (#310).

* Builds against Rcpp 0.11.1

* `select()` correctly works with the vars attribute (#309).

* Internal code is stricter when deciding if a data frame is grouped (#308):
  this avoids a number of situations which previously causedd .

* More data frame joins work with missing values in keys (#306).

# dplyr 0.1.2

## New features

* `select()` is substantially more powerful. You can use named arguments to
  rename existing variables, and new functions `starts_with()`, `ends_with()`,
  `contains()`, `matches()` and `num_range()` to select variables based on
  their names. It now also makes a shallow copy, substantially reducing its
  memory impact (#158, #172, #192, #232).

* `summarize()` added as alias for `summarise()` for people from countries
  that don't don't spell things correctly ;) (#245)

## Bug fixes

* `filter()` now fails when given anything other than a logical vector, and
  correctly handles missing values (#249). `filter.numeric()` proxies
  `stats::filter()` so you can continue to use `filter()` function with
  numeric inputs (#264).

* `summarise()` correctly uses newly created variables (#259).

* `mutate()` correctly propagates attributes (#265) and `mutate.data.frame()`
  correctly mutates the same variable repeatedly (#243).

* `lead()` and `lag()` preserve attributes, so they now work with
  dates, times and factors (#166).

* `n()` never accepts arguments (#223).

* `row_number()` gives correct results (#227).

* `rbind_all()` silently ignores data frames with 0 rows or 0 columns (#274).

* `group_by()` orders the result (#242). It also checks that columns
  are of supported types (#233, #276).

* The hybrid evaluator did not handle some expressions correctly, for
  example in `if(n() > 5) 1 else 2` the subexpression `n()` was not
  substituted correctly. It also correctly processes `$` (#278).

* `arrange()` checks that all columns are of supported types (#266). It also
  handles list columns (#282).

* Working towards Solaris compatibility.

* Benchmarking vignette temporarily disabled due to microbenchmark
  problems reported by BDR.

# dplyr 0.1.1

## Improvements

* new `location()` and `changes()` functions which provide more information
  about how data frames are stored in memory so that you can see what
  gets copied.

* renamed `explain_tbl()` to `explain()` (#182).

* `tally()` gains `sort` argument to sort output so highest counts
  come first (#173).

* `ungroup.grouped_df()`, `tbl_df()`, `as.data.frame.tbl_df()` now only
  make shallow copies of their inputs (#191).

* The `benchmark-baseball` vignette now contains fairer (including grouping
  times) comparisons with `data.table`. (#222)
  
## Bug fixes

* `filter()` (#221) and `summarise()` (#194) correctly propagate attributes.

* `summarise()` throws an error when asked to summarise an unknown variable
  instead of crashing (#208).

* `group_by()` handles factors with missing values (#183).

* `filter()` handles scalar results (#217) and better handles scoping, e.g.
  `filter(., variable)` where `variable` is defined in the function that calls
  `filter`. It also handles `T` and `F` as aliases to `TRUE` and `FALSE`
  if there are no `T` or `F` variables in the data or in the scope.

* `select.grouped_df` fails when the grouping variables are not included
  in the selected variables (#170)

* `all.equal.data.frame()` handles a corner case where the data frame has
  `NULL` names (#217)

* `mutate()` gives informative error message on unsupported types (#179)

* dplyr source package no longer includes pandas benchmark, reducing
  download size from 2.8 MB to 0.5 MB.

