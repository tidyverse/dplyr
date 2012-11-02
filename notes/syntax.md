# Syntax ideas

Common operations take the active form of the verb:

    selects()     
    summarises()   (aka summarizes)
    transforms()   (aka mutates)
    reorders()     (aka arranges)
    filters()      (aka subsets)

    takes()

    splits()
    applies()
    combines()

(`filters` is more obviously a verb than `subsets`)

## Combining operations

Individual operations are combined with `+` to form a compound operation:

    op <- 
      counts(by_var(cod, hod)) +
      filters(is.na(hod)) +
      splits(by_var(cod)) + 
        transforms(prop = freq / sum(freq) +
      combines() +
      arranges(desc(freq))

When combined with a data source, the result can be realised:

    source(deaths) + op

This gives a class hierarchy:

    op -> subset_op | filter_op | group_op | transform_op | order_op
    ops -> list of ops
    complete_ops -> ops + source

`+` combines classes as follows:

    op + op -> ops
    ops + op -> ops
    complete_ops + op -> complete_ops

    op + source -> complete_ops
    ops + source -> complete_ops
    complete_ops + source -> complete_ops

An complete operation has one data source and a list of operations. A partial operation (an op or ops) just has a list of operations.

## Splitting and combining

To operate "by" group, use `splits` and `combines`:

    splits(by_(cod)) + 
      transforms(prop = freq / sum(freq)) +
    combines()

If `applies` is not used, the last result will be combined automatically, so that the last operation can be abbreviated as:

    splits(by_(cod)) + 
      transforms(prop = freq / sum(freq))

The argument is a function that takes a data frame as input and returns a vector where each group has a common value. Some useful functions are included:

    by_var(mycol)
    by_(mycol) # an abbrevation

    x <- "mycol"
    by_name(x)

    by_rows()


## Arbitrary operations.

Functions that don't return a data frame use `applies`:

    baseball +
      splits(by_(id)) + 
      applies(lm, formula = g ~ year)

The compliment, `combines`, collapses the list back into a `data.frame`

    baseball +
      splits(by_(id)) + 
      applies(lm, formula = g ~ year) +
      combines(coef)

Like `plyr`, applies would support parallelisation with `foreach`.

## Planning/compilation

While operations are expressed in a fixed order, it might be more efficient to rearrange them.  This will be especially important for external data sources, since we can typically only do one pass in the external application before we get results back into R. 

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
    summarise + mutate    -> INTERPOLATE?
    summarise + subset    -> SELECT + HAVING / WHERE
    summarise + summarise -> MERGE (implicit AND)

    ops <- format(c("subset", "summarise", "mutate", "arrange"))
    cat(with(expand.grid(ops, ops), paste(Var1, "+", Var2, collapse = "\n")))

Need way to describe pattern matching to simplify. And standard method for running. Differs between data.frame (just apply each method in sequence, always avaialble as fallback), data.table, and sql.

## Realisation

The process of applying the operations to the data source is called realisation (because we're realising (in the CS sense) the value of the output). For data frame and data table sources, this is just a matter of applying each operation to the output from the previous. This is not so straighforward for (e.g.) SQL because the output of the first operation will be an R data frame, not a new table. 

    realise(complete_ops)
    # Which calls
    realise_op(source, ops)
    # so we can dispatch by source type

If the source is a data.frame, then keep a list of by groups. Push on to that list for every `splits` call, and pop for every `combines`.  When applies used, switch to a list + `lapply` and combine explicitly calls `rbind.fill`. Similarly strategy should work with data.table.

For out of memory datasets, will return a maximum number of rows (100,000 by default?), along with a warning message.  May consider supporting some sort of paging mechanism.

## Facades

Will still need some basic facades built on top for the most common operations: `summarise_by`, `subset_by`, `mutate_by` etc.  This would make it easier to dip your toe in, within having to move to a completely different way of thinking about data manipulation.

## Some examples

    # Equivalent to:
    source(deaths) +
      filters(is.na(hod)) +
      splits(by_(cod, hod)) +
        summarises(freq = count()) +
      combines() +
      arranges(desc(freq)) +
      splits(by_(cod)) + 
        transforms(prop = freq / sum(freq))

    source(deaths) +
      counts(by_(cod, hod)) +
      filters(is.na(hod)) +
      splits(by_(cod)) +
        splits(by_(hod))
          summarises(freq = count()) +
        combines() +
        transforms(prop = freq / sum(freq)) +
      combines() +
      arranges(desc(freq))

    baseball <- source(baseball)
    baseball + filters(year < 1900)
    baseball + by(id, filters(all(year < 1900)))
    baseball + by(id, filters(length(id) > 10)))

    baseball + 
      splits(by_(id)) + 
        transforms(cyear = year - min(year) + 1)) +
      combines() +
      filters(cyear == 1)

    # Which is equivalent to
    baseball + 
      splits(by_(id)) + 
        transforms(cyear = year - min(year) + 1)) +
        filters(cyear == 1) +
      combines()
