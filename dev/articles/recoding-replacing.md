# Recoding columns and replacing values

``` r
library(dplyr)
```

## Introduction

dplyr provides a family of functions for *recoding* columns and
*replacing* values within a column. These are extremely common
operations, so mastering this family can be a big productivity boost!

Before we begin, it’ll be helpful to define exactly what we mean by
recoding vs replacing:

- *Recoding* a column creates an entirely new column using values from
  an existing column. The new column may have a different type from the
  original column.

- *Replacing* values within a column partially updates an existing
  column with new values. The result has the same type as the original
  column.

The family of functions can be summarized by the following table:

|                           | **Recoding**                                                                                | **Replacing**                                                                                |
|---------------------------|---------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|
| **Match with conditions** | [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)         | [`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)       |
| **Match with values**     | [`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md) | [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md) |

This vignette walks through use cases for each of these functions, which
should help you build some intuition about when to use them.

## `case_when()`

[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
is the most general function in the family. It works by evaluating each
case sequentially and using the first match for each element to
determine the corresponding value in the output. To demonstrate, we’ll
look at a dataset of some 5k times in minutes:

``` r
set.seed(123)
racers <- tibble(
  id = seq_len(100),
  time = round(sample(1200:2100, size = 100, replace = TRUE) / 60, 2)
)
racers
#> # A tibble: 100 × 2
#>       id  time
#>    <int> <dbl>
#>  1     1  26.9
#>  2     2  27.7
#>  3     3  23.0
#>  4     4  28.8
#>  5     5  23.2
#>  6     6  33.6
#>  7     7  22.0
#>  8     8  25.0
#>  9     9  23.8
#> 10    10  24.0
#> # ℹ 90 more rows
```

We can use
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
to categorize these times into tiers:

``` r
tiers <- racers |>
  mutate(
    tier = case_when(
      time < 23 ~ "A",
      time < 27 ~ "B",
      time < 30 ~ "C",
      time < 33 ~ "D"
    )
  )

tiers
#> # A tibble: 100 × 3
#>       id  time tier 
#>    <int> <dbl> <chr>
#>  1     1  26.9 B    
#>  2     2  27.7 C    
#>  3     3  23.0 A    
#>  4     4  28.8 C    
#>  5     5  23.2 B    
#>  6     6  33.6 NA   
#>  7     7  22.0 A    
#>  8     8  25.0 B    
#>  9     9  23.8 B    
#> 10    10  24.0 B    
#> # ℹ 90 more rows
```

There’s a few things to note here:

- The *first* condition that is `TRUE` is used, i.e. a time of 21
  minutes meets all of the conditions, but would be placed in tier `A`
  because `time < 23` is listed first.

- Unmatched values fall through as `NA`. We have some racers above 33
  minutes that aren’t captured here!

There are a few options for dealing with unmatched locations. You can
leave them as `NA` if that makes sense for your use case, or you can
specify a `.default` value:

``` r
racers |>
  mutate(
    tier = case_when(
      time < 23 ~ "A",
      time < 27 ~ "B",
      time < 30 ~ "C",
      time < 33 ~ "D",
      .default = "unknown"
    )
  )
#> # A tibble: 100 × 3
#>       id  time tier   
#>    <int> <dbl> <chr>  
#>  1     1  26.9 B      
#>  2     2  27.7 C      
#>  3     3  23.0 A      
#>  4     4  28.8 C      
#>  5     5  23.2 B      
#>  6     6  33.6 unknown
#>  7     7  22.0 A      
#>  8     8  25.0 B      
#>  9     9  23.8 B      
#> 10    10  24.0 B      
#> # ℹ 90 more rows
```

If you are confident that you’ve captured every case, you can supply
`.unmatched = "error"` rather than `.default` and
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
will error if that assertion doesn’t hold. This is great for defensive
programming!

``` r
racers |>
  mutate(
    tier = case_when(
      time < 23 ~ "A",
      time < 27 ~ "B",
      time < 30 ~ "C",
      time < 33 ~ "D",
      .unmatched = "error"
    )
  )
#> Error in `mutate()`:
#> ℹ In argument: `tier = case_when(...)`.
#> Caused by error in `case_when()`:
#> ! Each location must be matched.
#> ✖ Locations 6, 22, 32, 34, 40, 44, 55, 56, 73, 79, 84, and 93 are unmatched.
```

Note that missing values must be explicitly handled when setting
`.unmatched = "error"`, even if that’s just `is.na(time) ~ NA`,
otherwise they will trigger the unmatched error.

## `replace_when()`

Let’s assume that some of our racers used banned running shoes and are
disqualified. Also, some racers had a false start and need to incur a 20
second (1/3 minute) penalty.

``` r
id_banned_shoes <- c(2, 10, 15, 32, 65)
id_false_start <- c(1, 2, 5, 20, 55, 74, 91)
```

We could add this information in a few ways. With
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md):

``` r
racers |>
  mutate(
    time = case_when(
      id %in% id_banned_shoes ~ NA,
      id %in% id_false_start ~ time + 1 / 3,
      .default = time
    )
  )
#> # A tibble: 100 × 2
#>       id  time
#>    <int> <dbl>
#>  1     1  27.2
#>  2     2  NA  
#>  3     3  23.0
#>  4     4  28.8
#>  5     5  23.6
#>  6     6  33.6
#>  7     7  22.0
#>  8     8  25.0
#>  9     9  23.8
#> 10    10  NA  
#> # ℹ 90 more rows
```

Or in two steps with
[`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md):

``` r
racers |>
  mutate(time = if_else(id %in% id_banned_shoes, NA, time)) |>
  mutate(time = if_else(id %in% id_false_start, time + 1 / 3, time))
#> # A tibble: 100 × 2
#>       id  time
#>    <int> <dbl>
#>  1     1  27.2
#>  2     2  NA  
#>  3     3  23.0
#>  4     4  28.8
#>  5     5  23.6
#>  6     6  33.6
#>  7     7  22.0
#>  8     8  25.0
#>  9     9  23.8
#> 10    10  NA  
#> # ℹ 90 more rows
```

Neither of these feel particularly elegant at expressing the *intent* of
this operation. All you’re trying to do is replace a few values of
`time`! We like to think of `time` as the *primary* input: `time` goes
in, and `time` comes out (slightly adjusted). But both
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
and [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
have `time` as their last input, making the intent a bit hard to
understand at first glance.

[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
lets you pull the primary input to the front (which also makes it
compatible with the pipe!), making the intent more clear:

``` r
racers |>
  mutate(
    time = time |>
      replace_when(
        id %in% id_banned_shoes ~ NA,
        id %in% id_false_start ~ time + 1 / 3
      )
  )
#> # A tibble: 100 × 2
#>       id  time
#>    <int> <dbl>
#>  1     1  27.2
#>  2     2  NA  
#>  3     3  23.0
#>  4     4  28.8
#>  5     5  23.6
#>  6     6  33.6
#>  7     7  22.0
#>  8     8  25.0
#>  9     9  23.8
#> 10    10  NA  
#> # ℹ 90 more rows
```

As a side note, you might have been tempted to reach for
[`base::replace()`](https://rdrr.io/r/base/replace.html) here, i.e. as:

``` r
racers |>
  mutate(time = base::replace(time, id %in% id_banned_shoes, NA)) |>
  mutate(time = base::replace(time, id %in% id_false_start, time + 1 / 3))
```

This actually doesn’t work! Replacing with `NA` does work, but
[`replace()`](https://rdrr.io/r/base/replace.html) requires that the
result of `time + 1 / 3` must be preemptively subset to the places where
the condition is true. You’d have to do something more complicated to
mimic
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md):

``` r
racers |>
  mutate(time = base::replace(time, id %in% id_banned_shoes, NA)) |>
  mutate(time = {
    loc <- id %in% id_false_start
    base::replace(time, loc, time[loc] + 1 / 3)
  })
#> # A tibble: 100 × 2
#>       id  time
#>    <int> <dbl>
#>  1     1  27.2
#>  2     2  NA  
#>  3     3  23.0
#>  4     4  28.8
#>  5     5  23.6
#>  6     6  33.6
#>  7     7  22.0
#>  8     8  25.0
#>  9     9  23.8
#> 10    10  NA  
#> # ℹ 90 more rows
```

### Type stability

Beyond readability, an important benefit of
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
(and
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md),
which we’ll see later) is that it is *type stable* on the column you are
modifying, which means that it can’t change types out from under you.

Type stability is particularly useful with factors. Taking another look
at our `tiers` of race times, imagine that some of the race times were
discovered to be faulty due to malfunctioning timers, and you need to
replace a few `id`s with the `unknown` level.

``` r
id_with_malfunction <- c(1, 5, 20, 50)

tiers <- racers |>
  mutate(
    tier = case_when(
      time < 23 ~ "A",
      time < 27 ~ "B",
      time < 30 ~ "C",
      time < 33 ~ "D",
      .default = "unknown"
    ) |>
      factor(levels = c("A", "B", "C", "D", "unknown"))
  )

tiers
#> # A tibble: 100 × 3
#>       id  time tier   
#>    <int> <dbl> <fct>  
#>  1     1  26.9 B      
#>  2     2  27.7 C      
#>  3     3  23.0 A      
#>  4     4  28.8 C      
#>  5     5  23.2 B      
#>  6     6  33.6 unknown
#>  7     7  22.0 A      
#>  8     8  25.0 B      
#>  9     9  23.8 B      
#> 10    10  24.0 B      
#> # ℹ 90 more rows
```

Note that the following
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
solution results in `tier` becoming a *character* column, losing its
factor class. This is due to the fact that
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
is a *recoding* function, it creates an entirely new column and doesn’t
know that you’re trying to retain existing type information.

``` r
tiers |>
  mutate(
    tier = case_when(id %in% id_with_malfunction ~ "unknown", .default = tier)
  )
#> # A tibble: 100 × 3
#>       id  time tier   
#>    <int> <dbl> <chr>  
#>  1     1  26.9 unknown
#>  2     2  27.7 C      
#>  3     3  23.0 A      
#>  4     4  28.8 C      
#>  5     5  23.2 unknown
#>  6     6  33.6 unknown
#>  7     7  22.0 A      
#>  8     8  25.0 B      
#>  9     9  23.8 B      
#> 10    10  24.0 B      
#> # ℹ 90 more rows
```

As a *replacing* function,
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
knows to be type stable on `tier`, and casts `"unknown"` to `tier`’s
factor type before performing the replacement:

``` r
tiers |>
  mutate(
    tier = tier |> replace_when(id %in% id_with_malfunction ~ "unknown")
  )
#> # A tibble: 100 × 3
#>       id  time tier   
#>    <int> <dbl> <fct>  
#>  1     1  26.9 unknown
#>  2     2  27.7 C      
#>  3     3  23.0 A      
#>  4     4  28.8 C      
#>  5     5  23.2 unknown
#>  6     6  33.6 unknown
#>  7     7  22.0 A      
#>  8     8  25.0 B      
#>  9     9  23.8 B      
#> 10    10  24.0 B      
#> # ℹ 90 more rows
```

## `recode_values()`

[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
and
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
both take *logical* vectors on the left-hand side of the formula. This
is very flexible, but sometimes these functions require a large amount
of repetition. Consider the following [Likert
scale](https://en.wikipedia.org/wiki/Likert_scale) scores. We’d like to
recode these from their numeric values to their character counterparts.

``` r
likert <- tibble(
  score = c(1, 2, 3, 4, 5, 2, 3, 1, 4)
)
```

We could certainly use a
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md):

``` r
likert |>
  mutate(
    score = case_when(
      score == 1 ~ "Strongly disagree",
      score == 2 ~ "Disagree",
      score == 3 ~ "Neutral",
      score == 4 ~ "Agree",
      score == 5 ~ "Strongly agree"
    )
  )
#> # A tibble: 9 × 1
#>   score            
#>   <chr>            
#> 1 Strongly disagree
#> 2 Disagree         
#> 3 Neutral          
#> 4 Agree            
#> 5 Strongly agree   
#> 6 Disagree         
#> 7 Neutral          
#> 8 Strongly disagree
#> 9 Agree
```

But `score ==` is repeated many times! If you find yourself using `==`
or `%in%` on the left-hand side in this manner, you likely want to use
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
instead. Rather than taking logical vectors,
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
takes *values* on the left-hand side to match against a single input
that you’ll provide as the first argument.

``` r
likert |>
  mutate(
    score = score |>
      recode_values(
        1 ~ "Strongly disagree",
        2 ~ "Disagree",
        3 ~ "Neutral",
        4 ~ "Agree",
        5 ~ "Strongly agree"
      )
  )
#> # A tibble: 9 × 1
#>   score            
#>   <chr>            
#> 1 Strongly disagree
#> 2 Disagree         
#> 3 Neutral          
#> 4 Agree            
#> 5 Strongly agree   
#> 6 Disagree         
#> 7 Neutral          
#> 8 Strongly disagree
#> 9 Agree
```

This removes all of the repetition, allowing you to focus on the
mapping. If you squint, the mapping should look roughly like a lookup
table between the numeric value and the likert encoding.
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
actually has a second interface that allows us to make this lookup table
representation even more explicit.

Using a
[`tribble()`](https://tibble.tidyverse.org/reference/tribble.html), we
can extract out the lookup table into its own standalone data frame.

``` r
lookup <- tribble(
  ~from , ~to                 ,
      1 , "Strongly disagree" ,
      2 , "Disagree"          ,
      3 , "Neutral"           ,
      4 , "Agree"             ,
      5 , "Strongly agree"
)
```

We can then utilize the alternative `from` and `to` arguments of
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
rather than supplying formulas to specify how the values should be
recoded:

``` r
likert |>
  mutate(score = recode_values(score, from = lookup$from, to = lookup$to))
#> # A tibble: 9 × 1
#>   score            
#>   <chr>            
#> 1 Strongly disagree
#> 2 Disagree         
#> 3 Neutral          
#> 4 Agree            
#> 5 Strongly agree   
#> 6 Disagree         
#> 7 Neutral          
#> 8 Strongly disagree
#> 9 Agree
```

Lifting the lookup table to the top of the file is particularly nice
when you have a long pipe chain. The details of the mapping get some
room to breathe, and in the pipe chain you can focus on the actual
manipulations you are trying to perform.

It’s also very common for your `lookup` table to exist in a CSV file
that you have to read in separately. In that case, you can replace the
[`tribble()`](https://tibble.tidyverse.org/reference/tribble.html) call
with:

``` r
lookup <- readr::read_csv("lookup.csv")
```

But everything else works the same. This would be quite hard to specify
with just the formula interface!

Like
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md),
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
also has `default` and `unmatched` arguments to handle unmatched
locations:

``` r
likert <- tibble(
  score = c(0, 1, 2, 2, 4, 5, 2, 3, 1, 4)
)

# Missed the `0`
likert |>
  mutate(
    score = score |>
      recode_values(
        from = lookup$from,
        to = lookup$to,
        unmatched = "error"
      )
  )
#> Error in `mutate()`:
#> ℹ In argument: `score = recode_values(score, from =
#>   lookup$from, to = lookup$to, unmatched = "error")`.
#> Caused by error in `recode_values()`:
#> ! Each location must be matched.
#> ✖ Location 1 is unmatched.
```

## `replace_values()`

As seen above, when replacing a few locations in a column using *logical
conditions*, we reached for
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
rather than
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md).
Similarly, when replacing a few locations using *values* to match
against, it’s best to use
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
over
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md).

Imagine we’d like to collapse some, but not all, of these school names
into common buckets:

``` r
schools <- tibble(
  name = c(
    "UNC",
    "Chapel Hill",
    NA,
    "Duke",
    "Duke University",
    "UNC",
    "NC State",
    "ECU"
  )
)
```

We could use
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md):

``` r
schools |>
  mutate(
    name = recode_values(
      name,
      c("UNC", "Chapel Hill") ~ "UNC Chapel Hill",
      c("Duke", "Duke University") ~ "Duke",
      default = name
    )
  )
#> # A tibble: 8 × 1
#>   name           
#>   <chr>          
#> 1 UNC Chapel Hill
#> 2 UNC Chapel Hill
#> 3 NA             
#> 4 Duke           
#> 5 Duke           
#> 6 UNC Chapel Hill
#> 7 NC State       
#> 8 ECU
```

But this “partial update by value” is so common that it really deserves
its own name that doesn’t require you to specify `default`. For that, we
have
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md):

``` r
schools |>
  mutate(
    name = name |>
      replace_values(
        c("UNC", "Chapel Hill") ~ "UNC Chapel Hill",
        c("Duke", "Duke University") ~ "Duke"
      )
  )
#> # A tibble: 8 × 1
#>   name           
#>   <chr>          
#> 1 UNC Chapel Hill
#> 2 UNC Chapel Hill
#> 3 NA             
#> 4 Duke           
#> 5 Duke           
#> 6 UNC Chapel Hill
#> 7 NC State       
#> 8 ECU
```

Like
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md),
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
has an alternative `from` and `to` API that works well with lookup
tables and allows you to move your mapping out of the pipe chain:

``` r
lookup <- tribble(
  ~from             , ~to               ,
  "UNC"             , "UNC Chapel Hill" ,
  "Chapel Hill"     , "UNC Chapel Hill" ,
  "Duke"            , "Duke"            ,
  "Duke University" , "Duke"
)

schools |>
  mutate(name = replace_values(name, from = lookup$from, to = lookup$to))
#> # A tibble: 8 × 1
#>   name           
#>   <chr>          
#> 1 UNC Chapel Hill
#> 2 UNC Chapel Hill
#> 3 NA             
#> 4 Duke           
#> 5 Duke           
#> 6 UNC Chapel Hill
#> 7 NC State       
#> 8 ECU
```

An extremely neat feature of the `from` and `to` API is that they also
take *lists* of vectors that describe the mapping, which has been
designed to work elegantly with the fact that
[`tribble()`](https://tibble.tidyverse.org/reference/tribble.html) can
create list columns, allowing you to further collapse this lookup table:

``` r
# Condensed lookup table with a `many:1` mapping per row
lookup <- tribble(
  ~from                        , ~to               ,
  c("UNC", "Chapel Hill")      , "UNC Chapel Hill" ,
  c("Duke", "Duke University") , "Duke"
)

# Note that `from` is a list column
lookup
#> # A tibble: 2 × 2
#>   from      to             
#>   <list>    <chr>          
#> 1 <chr [2]> UNC Chapel Hill
#> 2 <chr [2]> Duke

lookup$from
#> [[1]]
#> [1] "UNC"         "Chapel Hill"
#> 
#> [[2]]
#> [1] "Duke"            "Duke University"

# Works the same as before
schools |>
  mutate(name = replace_values(name, from = lookup$from, to = lookup$to))
#> # A tibble: 8 × 1
#>   name           
#>   <chr>          
#> 1 UNC Chapel Hill
#> 2 UNC Chapel Hill
#> 3 NA             
#> 4 Duke           
#> 5 Duke           
#> 6 UNC Chapel Hill
#> 7 NC State       
#> 8 ECU
```

## Comparisons

We’ll end this vignette with some comparisons of the recoding and
replacing family to other dplyr functions and to other technologies,
like SQL.

### `if_else()`

[`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md) is a
type of recoding function, as it creates an entirely new column. In
fact, it’s closely tied to
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md):

``` r
if_else(condition, true, false, missing)

case_when(
  condition ~ true,
  !condition ~ false,
  is.na(condition) ~ missing
)
```

Similar to
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md),
[`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
doesn’t offer type stability on any particular input. The output’s type
is computed as the common type of `true`, `false`, and `missing`. If you
find yourself writing an
[`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md)
where the purpose is to partially update an existing column, consider
using
[`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
instead for clarity and type stability:

``` r
x <- if_else(x > 5, new, x)

# Type stable on `x`.
# Intent of "partially updating" `x` is clear.
# Pipe friendly.
x <- x |> replace_when(x > 5 ~ new)
```

### `coalesce()`

For converting from `NA` to some other value, the most common cases of
[`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
are often a
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
call in disguise:

``` r
x <- c(1, 2, NA, 3, NA, 5)
y <- c(0, 3, 1, 4, 6, 7)

coalesce(x, 0)
#> [1] 1 2 0 3 0 5
replace_values(x, NA ~ 0)
#> [1] 1 2 0 3 0 5

coalesce(x, y)
#> [1] 1 2 1 3 6 5
replace_values(x, NA ~ y)
#> [1] 1 2 1 3 6 5
```

And with
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
you can replace any value, not just `NA`.

### `na_if()`

For converting from a problematic value to `NA`,
[`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
is a more flexible (and likely more intuitive) alternative to
[`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md):

``` r
x <- c(1, 2, 0, -99, 12)

# To convert `0` and `-99` to `NA`, you have to do it in two calls
x |> na_if(0) |> na_if(-99)
#> [1]  1  2 NA NA 12

x |> replace_values(from = c(0, -99), to = NA)
#> [1]  1  2 NA NA 12
```

### SQL

[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
is an R equivalent of SQL’s [Searched
`CASE`](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/case-transact-sql?view=sql-server-ver17#syntax)
statement:

``` r
case_when(
  x < 100 ~ this,
  x < 20 ~ that,
  .default = default
)
```

``` sql
CASE
  WHEN x < 100 THEN this
  WHEN x < 20 THEN that
  ELSE default
END
```

And dbplyr will translate a
[`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
to this form!

[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
is an R equivalent of SQL’s [Simple
`CASE`](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/case-transact-sql?view=sql-server-ver17#syntax)
statement:

``` r
recode_values(
  x,
  "E" ~ "East",
  "W" ~ "West",
  "N" ~ "North",
  "S" ~ "South",
  .default = "Unknown"
)
```

``` sql
CASE x
  WHEN 'E' THEN 'East'
  WHEN 'W' THEN 'West'
  WHEN 'N' THEN 'North'
  WHEN 'S' THEN 'South'
  ELSE 'Unknown'
END
```

As of dbplyr 2.5.1, we don’t currently have a translation for
[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
since it is so new, but we expect to have one soon.
