# Syntax ideas

Common operations need to be special cased for performance (count, subset, select, summarise, mutate, arrange) - these also all return data frames.

Need alternative syntax for functions that return a list (with special form if you can apriori supply how many rows they will return).  `each_d` and `each_l` ?

Operations are expressed in a linear manner, but the query planner might re-arrange/combine if it can do so efficiently.  This will be especially important for external data sources. Only need to merge first sequence of operations: the rest can be done in memory.

    arrange   + arrange   -> MERGE
    arrange   + mutate    ->
    arrange   + subset    -> WHERE + ORDER BY
    arrange   + summarise -> 
    mutate    + arrange   ->
    mutate    + mutate    -> MERGE
    mutate    + subset    -> SELECT + WHERE
    mutate    + summarise -> END
    subset    + arrange   ->
    subset    + mutate    ->
    subset    + subset    -> MERGE
    subset    + summarise ->
    summarise + arrange   ->
    summarise + mutate    ->
    summarise + subset    -> SELECT + HAVING / WHERE
    summarise + summarise -> MERGE (implicit AND)

    ops <- format(c("subset", "summarise", "mutate", "arrange"))
    cat(with(expand.grid(ops, ops), paste(Var1, "+", Var2, collapse = "\n")))

Need data structure to represent data source and sequence of actions. Need way to describe pattern matching to simplify. And standard method for running.  Differs between data.frame (just apply each method in sequence, always avaialble as fallback), data.table, and sql.

Need to distinguish between regular (immediate) and dply (delayed) operations.


    deaths +
      count(c("cod", "hod")) +
      subset(is.na(hod)) +
      by("cod") + transform(prop = freq / sum(freq)) + collapse()

    baseball +
      by("id", apply(lm, formula = g ~ year))

    baseball +
      by("id", apply(lm, formula = g ~ year)) +
      collapse(coef)

    baseball + subset(year < 1900)
    baseball + by("id", subset(all(year < 1900)))
    baseball + by("id", subset(length(id) > 10)))

    baseball + 
      by("id", mutate(cyear = year - min(year) + 1)) +
      subset(cyear == 1)

    baseball + 
      by("id", {
        mutate(cyear = year - min(year) + 1)) + 
        subset(cyear == 1)
      })


    _select()
    _summarise()
    _mutate()
    _arrange()
    _subset()

    _by(grouping thing, {
      _select()
      _summarise()
      _mutate()
      _arrange()
      _subset()
    })

    _apply(general function)
    _collapse() ?

