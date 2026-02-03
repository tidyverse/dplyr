# Recode and replace values

`recode_values()` and `replace_values()` provide two ways to map old
values to new values. They work by matching values against `x` and using
the first match to determine the corresponding value in the output
vector. You can also think of these functions as a way to use a lookup
table to recode a vector.

- Use `recode_values()` when creating an entirely new vector.

- Use `replace_values()` when partially updating an existing vector.

If you are just replacing a few values within an existing vector, then
`replace_values()` is always a better choice because it is type stable
and better expresses intent.

A major difference between the two functions is what happens when no
cases match:

- `recode_values()` falls through to a `default`.

- `replace_values()` retains the original values from `x`.

These functions have two mutually exclusive ways to use them:

- A formula-based approach, i.e.
  `recode_values(x, from1 ~ to1, from2 ~ to2)`, similar to
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.md),
  which is useful when you have a small number of cases.

- A vector-based approach, i.e.
  `recode_values(x, from = from, to = to)`, which is useful when you
  have a pre-built lookup table (which may come from an external source,
  like a CSV file).

See
[`vignette("recoding-replacing")`](https://dplyr.tidyverse.org/articles/recoding-replacing.md)
for more examples.

## Usage

``` r
recode_values(
  x,
  ...,
  from = NULL,
  to = NULL,
  default = NULL,
  unmatched = "default",
  ptype = NULL
)

replace_values(x, ..., from = NULL, to = NULL)
```

## Arguments

- x:

  A vector.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas. The left hand side (LHS) determines
  which values match this case. The right hand side (RHS) provides the
  replacement value.

  - The LHS inputs can be any size, but will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to the type of `x`.

  - The RHS inputs will be
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
    to the same size as `x`. For `recode_values()` they will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to their common type, and for `replace_values()` they will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to the type of `x`.

  `NULL` inputs are ignored.

  Mutually exclusive with `from` and `to`.

- from:

  Values to look up in `x` and map to values in `to`.

  Typically this is a single vector of any size that is
  [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html) to
  the type of `x`. For more advanced usage, this can be a list of
  vectors of any size each of which are
  [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html) to
  the type of `x`.

  Mutually exclusive with `...`.

- to:

  Values that `from` map to.

  Typically this is a single vector that is
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `from`. For more advanced usage, this can be a list of
  vectors each of which are
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `x`.

  Mutually exclusive with `...`.

- default:

  Default value to use when there is a value present in `x` that is
  unmatched by a value in `from`.

  By default, a missing value is used as the default value.

  If supplied, will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `x`.

  Can only be set when `unmatched = "default"`.

- unmatched:

  Handling of unmatched locations.

  One of:

  - `"default"` to use `default` in unmatched locations.

  - `"error"` to error when there are unmatched locations.

- ptype:

  An optional override for the output type, which is usually computed as
  the common type of `to` and `default`.

## Value

A vector the same size as `x`.

- For `recode_values()`, the type of the output is computed as the
  common type of `to` and `default`, unless overridden by `ptype`. The
  names of the output come from the names of `to` and `default`.

- For `replace_values()`, the type of the output will have the same type
  as `x`. The names of the output will be the same as the names of `x`.

## See also

[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.md),
[`vctrs::vec_recode_values()`](https://vctrs.r-lib.org/reference/vec-recode-and-replace.html)

## Examples

``` r
x <- c("NC", "NYC", "CA", NA, "NYC", "Unknown")

# `recode_values()` is useful for fully recoding from one set of values to
# another, creating an entirely new vector in the process. Note that any
# unmatched values result in `NA`, or a `default` value.
recode_values(
  x,
  "NC" ~ "North Carolina",
  "NYC" ~ "New York",
  "CA" ~ "California"
)
#> [1] "North Carolina" "New York"       "California"     NA              
#> [5] "New York"       NA              

recode_values(
  x,
  "NC" ~ "North Carolina",
  "NYC" ~ "New York",
  "CA" ~ "California",
  default = "<not recorded>"
)
#> [1] "North Carolina" "New York"       "California"     "<not recorded>"
#> [5] "New York"       "<not recorded>"

# `replace_values()` is useful for updating an existing vector, tweaking a
# few values along the way
replace_values(x, "NYC" ~ "NY")
#> [1] "NC"      "NY"      "CA"      NA        "NY"      "Unknown"

# `replace_values()` is particularly nice for replacing `NA`s with values...
replace_values(x, NA ~ "Unknown (NA)")
#> [1] "NC"           "NYC"          "CA"           "Unknown (NA)"
#> [5] "NYC"          "Unknown"     
# ...or values with `NA`s
replace_values(x, "Unknown" ~ NA)
#> [1] "NC"  "NYC" "CA"  NA    "NYC" NA   

# Multiple values can be grouped within a single left-hand side to normalize
# all problematic values at once
replace_values(x, c(NA, "Unknown") ~ "<not recorded>")
#> [1] "NC"             "NYC"            "CA"             "<not recorded>"
#> [5] "NYC"            "<not recorded>"

# ---------------------------------------------------------------------------
# Lookup tables

# `recode_values()` works with more than just character vectors. Imagine you
# have this series of Likert Scale scores, which is a scoring system that is
# ordered from 1-5.
data <- tibble(
  score = c(1, 2, 3, 4, 5, 2, 3, 1, 4)
)

# To recode each `score` to its corresponding Likert Score label, you may
# initially be inclined to reach for `case_when()`
data |>
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

# While this works, it can be written more efficiently using
# `recode_values()`
data |>
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

# `recode_values()` actually has two mutually exclusive APIs. The formula API
# used above, which is like `case_when()`, and a lookup style API that uses
# `from` and `to` arguments. The lookup API is even better suited for this
# problem, because we can move the mapping outside of the `mutate()` call
# into a standalone lookup table. You could even imagine reading this
# `likert` lookup table in from a separate CSV file.
likert <- tribble(
  ~from, ~to,
  1, "Strongly disagree",
  2, "Disagree",
  3, "Neutral",
  4, "Agree",
  5, "Strongly agree"
)

data |>
  mutate(score = recode_values(score, from = likert$from, to = likert$to))
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

# You can utilize the same lookup table across multiple columns by using
# `across()`
data_months <- tibble(
  score_january = c(1, 2, 3, 4, 5, 2, 3, 1, 4),
  score_february = c(4, 2, 1, 2, 1, 5, 2, 4, 4)
)

data_months |>
  mutate(across(
    starts_with("score"),
    ~ recode_values(.x, from = likert$from, to = likert$to)
  ))
#> # A tibble: 9 × 2
#>   score_january     score_february   
#>   <chr>             <chr>            
#> 1 Strongly disagree Agree            
#> 2 Disagree          Disagree         
#> 3 Neutral           Strongly disagree
#> 4 Agree             Disagree         
#> 5 Strongly agree    Strongly disagree
#> 6 Disagree          Strongly agree   
#> 7 Neutral           Disagree         
#> 8 Strongly disagree Agree            
#> 9 Agree             Agree            

# The `unmatched` argument allows you to assert that you believe that you've
# recoded all of the cases and will error if you've missed one, adding an
# extra layer of safety
data_with_zero <- add_row(data, score = 0)

try({
  recode_values(
    data_with_zero$score,
    from = likert$from,
    to = likert$to,
    unmatched = "error"
  )
})
#> Error in recode_values(data_with_zero$score, from = likert$from, to = likert$to,  : 
#>   Each location must be matched.
#> ✖ Location 10 is unmatched.

# Note that missing values are considered unmatched. If you expect missing
# values, you'll need to handle them explicitly in your lookup table.
data_with_missing <- add_row(data, score = NA)

try({
  recode_values(
    data_with_missing$score,
    from = likert$from,
    to = likert$to,
    unmatched = "error"
  )
})
#> Error in recode_values(data_with_missing$score, from = likert$from, to = likert$to,  : 
#>   Each location must be matched.
#> ✖ Location 10 is unmatched.

likert <- add_row(likert, from = NA, to = NA)

recode_values(
  data_with_missing$score,
  from = likert$from,
  to = likert$to,
  unmatched = "error"
)
#>  [1] "Strongly disagree" "Disagree"          "Neutral"          
#>  [4] "Agree"             "Strongly agree"    "Disagree"         
#>  [7] "Neutral"           "Strongly disagree" "Agree"            
#> [10] NA                 

# ------------------------------------------------------------------------------
# Lists of vectors

# In some cases, your mapping may collapse multiple groups together into a
# single value. For example, here we'd like to standardize the school names.
schools <- c(
  "UNC",
  "Chapel Hill",
  NA,
  "Duke",
  "Duke University",
  "UNC",
  "NC State",
  "ECU",
  "East Carolina"
)

# This `tribble()` is more complex than it may appear, it actually
# creates a list column!
standardized <- tribble(
  ~from,                        ~to,
  c("UNC", "Chapel Hill"),      "UNC",
  c("Duke", "Duke University"), "Duke",
  c("NC State"),                "NC State",
  c("ECU", "East Carolina"),    "ECU",
  NA,                           NA
)

standardized
#> # A tibble: 5 × 2
#>   from      to      
#>   <list>    <chr>   
#> 1 <chr [2]> UNC     
#> 2 <chr [2]> Duke    
#> 3 <chr [1]> NC State
#> 4 <chr [2]> ECU     
#> 5 <lgl [1]> NA      
standardized$from
#> [[1]]
#> [1] "UNC"         "Chapel Hill"
#> 
#> [[2]]
#> [1] "Duke"            "Duke University"
#> 
#> [[3]]
#> [1] "NC State"
#> 
#> [[4]]
#> [1] "ECU"           "East Carolina"
#> 
#> [[5]]
#> [1] NA
#> 

# `recode_values()` treats a list `from` value as a list of vectors, where
# any match within one of the vectors is mapped to its corresponding `to`
# value
recode_values(
  schools,
  from = standardized$from,
  to = standardized$to,
  unmatched = "error"
)
#> [1] "UNC"      "UNC"      NA         "Duke"     "Duke"     "UNC"     
#> [7] "NC State" "ECU"      "ECU"     

# This formula based approach is equivalent, but the lookup based approach is
# nicer because the lookup table can be defined separately
recode_values(
  schools,
  c("UNC", "Chapel Hill") ~ "UNC",
  c("Duke", "Duke University") ~ "Duke",
  c("NC State") ~ "NC State",
  c("ECU", "East Carolina") ~ "ECU",
  NA ~ NA,
  unmatched = "error"
)
#> [1] "UNC"      "UNC"      NA         "Duke"     "Duke"     "UNC"     
#> [7] "NC State" "ECU"      "ECU"     
```
