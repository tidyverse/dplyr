# A general vectorised `switch()`

**\[deprecated\]**

`case_match()` is deprecated. Please use
[`recode_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.md)
and
[`replace_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.md)
instead, which are more powerful, have more intuitive names, and have
better safety. In addition to the familiar two-sided formula interface,
these functions also have `from` and `to` arguments which allow you to
incorporate a lookup table into the recoding process.

This function allows you to vectorise multiple
[`switch()`](https://rdrr.io/r/base/switch.html) statements. Each case
is evaluated sequentially and the first match for each element
determines the corresponding value in the output vector. If no cases
match, the `.default` is used.

## Usage

``` r
case_match(.x, ..., .default = NULL, .ptype = NULL)
```

## Arguments

- .x:

  A vector to match against.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas: `old_values ~ new_value`. The right
  hand side (RHS) determines the output value for all values of `.x`
  that match the left hand side (LHS).

  The LHS must evaluate to the same type of vector as `.x`. It can be
  any length, allowing you to map multiple `.x` values to the same RHS
  value. If a value is repeated in the LHS, i.e. a value in `.x` matches
  to multiple cases, the first match is used.

  The RHS inputs will be coerced to their common type. Each RHS input
  will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `.x`.

- .default:

  The value used when values in `.x` aren't matched by any of the LHS
  inputs. If `NULL`, the default, a missing value will be used.

  `.default` is
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `.x`.

- .ptype:

  An optional prototype declaring the desired output type. If not
  supplied, the output type will be taken from the common type of all
  RHS inputs and `.default`.

## Value

A vector with the same size as `.x` and the same type as the common type
of the RHS inputs and `.default` (if not overridden by `.ptype`).

## Examples

``` r
# `case_match()` is deprecated and has been replaced by `recode_values()` and
# `replace_values()`

x <- c("a", "b", "a", "d", "b", NA, "c", "e")

# `recode_values()` is a 1:1 replacement for `case_match()`
case_match(
  x,
  "a" ~ 1,
  "b" ~ 2,
  "c" ~ 3,
  "d" ~ 4
)
#> Warning: `case_match()` was deprecated in dplyr 1.2.0.
#> ℹ Please use `recode_values()` instead.
#> [1]  1  2  1  4  2 NA  3 NA
recode_values(
  x,
  "a" ~ 1,
  "b" ~ 2,
  "c" ~ 3,
  "d" ~ 4
)
#> [1]  1  2  1  4  2 NA  3 NA

# `recode_values()` has an additional `unmatched` argument to help you catch
# missed mappings
try(recode_values(
  x,
  "a" ~ 1,
  "b" ~ 2,
  "c" ~ 3,
  "d" ~ 4,
  unmatched = "error"
))
#> Error in recode_values(x, "a" ~ 1, "b" ~ 2, "c" ~ 3, "d" ~ 4, unmatched = "error") : 
#>   Each location must be matched.
#> ✖ Locations 6 and 8 are unmatched.

# `recode_values()` also has additional `from` and `to` arguments, which are
# useful when your lookup table is defined elsewhere (for example, it could
# be read in from a CSV file). This is very difficult to do with
# `case_match()`!
lookup <- tribble(
  ~from, ~to,
  "a", 1,
  "b", 2,
  "c", 3,
  "d", 4
)

recode_values(x, from = lookup$from, to = lookup$to)
#> [1]  1  2  1  4  2 NA  3 NA

# Both `case_match()` and `recode_values()` work with more than just
# character inputs:
y <- as.integer(c(1, 2, 1, 3, 1, NA, 2, 4))

case_match(
  y,
  c(1, 3) ~ "odd",
  c(2, 4) ~ "even",
  .default = "missing"
)
#> [1] "odd"     "even"    "odd"     "odd"     "odd"     "missing"
#> [7] "even"    "even"   
recode_values(
  y,
  c(1, 3) ~ "odd",
  c(2, 4) ~ "even",
  default = "missing"
)
#> [1] "odd"     "even"    "odd"     "odd"     "odd"     "missing"
#> [7] "even"    "even"   

# Or with a lookup table
lookup <- tribble(
  ~from,   ~to,
  c(1, 3), "odd",
  c(2, 4), "even"
)
recode_values(y, from = lookup$from, to = lookup$to, default = "missing")
#> [1] "odd"     "even"    "odd"     "odd"     "odd"     "missing"
#> [7] "even"    "even"   

# `replace_values()` is a convenient way to replace selected values, leaving
# everything else as is. It's similar to `case_match(y, .default = y)`.
replace_values(y, NA ~ 0)
#> [1] 1 2 1 3 1 0 2 4
case_match(y, NA ~ 0, .default = y)
#> [1] 1 2 1 3 1 0 2 4

# Notably, `replace_values()` is type stable, which means that `y` can't
# change types out from under you, unlike with `case_match()`!
typeof(y)
#> [1] "integer"
typeof(replace_values(y, NA ~ 0))
#> [1] "integer"
typeof(case_match(y, NA ~ 0, .default = y))
#> [1] "double"

# We believe that `replace_values()` better expresses intent when doing a
# partial replacement. Compare these two `mutate()` calls, each with the
# goals of:
# - Replace missings in `hair_color`
# - Replace some of the `species`
starwars |>
  mutate(
    hair_color = case_match(hair_color, NA ~ "unknown", .default = hair_color),
    species = case_match(
      species,
      "Human" ~ "Humanoid",
      "Droid" ~ "Robot",
      c("Wookiee", "Ewok") ~ "Hairy",
      .default = species
    ),
    .keep = "used"
  )
#> # A tibble: 87 × 2
#>    hair_color    species 
#>    <chr>         <chr>   
#>  1 blond         Humanoid
#>  2 unknown       Robot   
#>  3 unknown       Robot   
#>  4 none          Humanoid
#>  5 brown         Humanoid
#>  6 brown, grey   Humanoid
#>  7 brown         Humanoid
#>  8 unknown       Robot   
#>  9 black         Humanoid
#> 10 auburn, white Humanoid
#> # ℹ 77 more rows

updates <- tribble(
  ~from,                ~to,
  "Human",              "Humanoid",
  "Droid",              "Robot",
  c("Wookiee", "Ewok"), "Hairy"
)

starwars |>
  mutate(
    hair_color = replace_values(hair_color, NA ~ "unknown"),
    species = replace_values(species, from = updates$from, to = updates$to),
    .keep = "used"
  )
#> # A tibble: 87 × 2
#>    hair_color    species 
#>    <chr>         <chr>   
#>  1 blond         Humanoid
#>  2 unknown       Robot   
#>  3 unknown       Robot   
#>  4 none          Humanoid
#>  5 brown         Humanoid
#>  6 brown, grey   Humanoid
#>  7 brown         Humanoid
#>  8 unknown       Robot   
#>  9 black         Humanoid
#> 10 auburn, white Humanoid
#> # ℹ 77 more rows
```
