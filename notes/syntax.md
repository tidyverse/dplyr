# Syntax ideas

Common operations take the active form of the verb:

    selects()     
    summarises()   (aka summarizes)
    transforms()   (aka mutates)
    reorders()     (aka arranges)
    filters()      (aka subsets)

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

## Grouping

In `groupwise` (an adverb), you can specify group as:

    by_var(mycol)
    by_(mycol) # perhaps an abbrevation

    x <- "mycol"
    by_name(x)

    by_rows()

You can also provide your own function that takes a data frame as input and returns a single vector where each group has a common value.

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

## Some examples

    # Equivalent to:
    source(deaths) +
      filters(is.na(hod)) +
      splits(by_(cod, hod)) +
        summarises(freq = count()) +
      combines() +
      arrange(desc(freq)) +
      splits(by_cod) + 
        by(var(cod), transforming(prop = freq / sum(freq)) 

    source(deaths) +
      counting(by_var(cod, hod)) +
      filters(is.na(hod)) +
      splits(by_var(cod)) +
        splits(by_var(hod))
          summarises(freq = count()) +
        combines() +
        transforming(prop = freq / sum(freq)) +
      combines() +
      arranging(desc(freq))

    baseball <- source(baseball)
    baseball + filters(year < 1900)
    baseball + by(id, filters(all(year < 1900)))
    baseball + by(id, filters(length(id) > 10)))

    baseball + 
      splits(by_(id)) + 
        transforming(cyear = year - min(year) + 1)) +
      combines() +
      subset(cyear == 1)

    # Which is equivalent to
    baseball + 
      splits(by_(id)) + 
        transforming(cyear = year - min(year) + 1)) +
        subset(cyear == 1) +
      combines()
