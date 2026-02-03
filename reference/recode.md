# Recode values

**\[superseded\]**

`recode()` is superseded in favor of
[`recode_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.md)
and
[`replace_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.md),
which are more general and have a much better interface.
`recode_factor()` is also superseded, however, its direct replacement is
not currently available but will eventually live in
[forcats](https://forcats.tidyverse.org/). For creating new variables
based on logical vectors, use
[`if_else()`](https://dplyr.tidyverse.org/reference/if_else.md). For
even more complicated criteria, use
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.md).

`recode()` is a vectorised version of
[`switch()`](https://rdrr.io/r/base/switch.html): you can replace
numeric values based on their position or their name, and character or
factor values only by their name. This is an S3 generic: dplyr provides
methods for numeric, character, and factors. You can use `recode()`
directly with factors; it will preserve the existing order of levels
while changing the values. Alternatively, you can use `recode_factor()`,
which will change the order of levels to match the order of
replacements.

## Usage

``` r
recode(.x, ..., .default = NULL, .missing = NULL)

recode_factor(.x, ..., .default = NULL, .missing = NULL, .ordered = FALSE)
```

## Arguments

- .x:

  A vector to modify

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Replacements. For character and factor `.x`, these should be named and
  replacement is based only on their name. For numeric `.x`, these can
  be named or not. If not named, the replacement is done based on
  position i.e. `.x` represents positions to look for in replacements.
  See examples.

  When named, the argument names should be the current values to be
  replaced, and the argument values should be the new (replacement)
  values.

  All replacements must be the same type, and must have either length
  one or the same length as `.x`.

- .default:

  If supplied, all values not otherwise matched will be given this
  value. If not supplied and if the replacements are the same type as
  the original values in `.x`, unmatched values are not changed. If not
  supplied and if the replacements are not compatible, unmatched values
  are replaced with `NA`.

  `.default` must be either length 1 or the same length as `.x`.

- .missing:

  If supplied, any missing values in `.x` will be replaced by this
  value. Must be either length 1 or the same length as `.x`.

- .ordered:

  If `TRUE`, `recode_factor()` creates an ordered factor.

## Value

A vector the same length as `.x`, and the same type as the first of
`...`, `.default`, or `.missing`. `recode_factor()` returns a factor
whose levels are in the same order as in `...`. The levels in `.default`
and `.missing` come last.

## See also

[`recode_values()`](https://dplyr.tidyverse.org/reference/recode-and-replace-values.md)

## Examples

``` r
set.seed(1234)

x <- sample(c("a", "b", "c"), 10, replace = TRUE)

# `recode()` is superseded by `recode_values()` and `replace_values()`

# If you are fully recoding a vector use `recode_values()`
recode(x, a = "Apple", b = "Banana", .default = NA_character_)
#>  [1] "Banana" "Banana" "Apple"  NA       "Apple"  "Apple"  "Banana"
#>  [8] "Banana" NA       "Banana"
recode_values(x, "a" ~ "Apple", "b" ~ "Banana")
#>  [1] "Banana" "Banana" "Apple"  NA       "Apple"  "Apple"  "Banana"
#>  [8] "Banana" NA       "Banana"

# With a default
recode(x, a = "Apple", b = "Banana", .default = "unknown")
#>  [1] "Banana"  "Banana"  "Apple"   "unknown" "Apple"   "Apple"  
#>  [7] "Banana"  "Banana"  "unknown" "Banana" 
recode_values(x, "a" ~ "Apple", "b" ~ "Banana", default = "unknown")
#>  [1] "Banana"  "Banana"  "Apple"   "unknown" "Apple"   "Apple"  
#>  [7] "Banana"  "Banana"  "unknown" "Banana" 

# If you are partially updating a vector and want to retain the original
# vector's values in locations you don't make a replacement, use
# `replace_values()`
recode(x, a = "Apple", b = "Banana")
#>  [1] "Banana" "Banana" "Apple"  "c"      "Apple"  "Apple"  "Banana"
#>  [8] "Banana" "c"      "Banana"
replace_values(x, "a" ~ "Apple", "b" ~ "Banana")
#>  [1] "Banana" "Banana" "Apple"  "c"      "Apple"  "Apple"  "Banana"
#>  [8] "Banana" "c"      "Banana"

# `replace_values()` is easier to use with numeric vectors, because you don't
# need to turn the numeric values into names
y <- c(1:4, NA)
recode(y, `2` = 20L, `4` = 40L)
#> [1]  1 20  3 40 NA
replace_values(y, 2 ~ 20L, 4 ~ 40L)
#> [1]  1 20  3 40 NA

# `recode()` is particularly confusing because it tries to handle both
# full recodings to new vector types and partial updating of an existing
# vector. With the above example, using doubles (20) rather than integers
# (20L) results in a warning from `recode()`, because it thinks you are
# doing a full recode and missed a case. `replace_values()` is type stable
# on `y` and will instead coerce the double values to integer.
recode(y, `2` = 20, `4` = 40)
#> Warning: Unreplaced values treated as NA as `.x` is not compatible.
#> Please specify replacements exhaustively or supply `.default`.
#> [1] NA 20 NA 40 NA
replace_values(y, 2 ~ 20, 4 ~ 40)
#> [1]  1 20  3 40 NA

# This also makes `replace_values()` much safer. If you provide
# incompatible types, it will error.
recode(y, `2` = "20", `4` = "40")
#> Warning: Unreplaced values treated as NA as `.x` is not compatible.
#> Please specify replacements exhaustively or supply `.default`.
#> [1] NA   "20" NA   "40" NA  
try(replace_values(y, 2 ~ "20", 4 ~ "40"))
#> Error in replace_values(y, 2 ~ "20", 4 ~ "40") : 
#>   Can't convert `..1 (right)` <character> to <integer>.

# If you were trying to fully recode the vector and want a different output
# type, use `recode_values()`
recode_values(y, 2 ~ "20", 4 ~ "40")
#> [1] NA   "20" NA   "40" NA  

# And if you want to ensure you don't miss a case, use `unmatched`, which
# errors rather than warns
try(recode_values(y, 2 ~ "20", 4 ~ "40", unmatched = "error"))
#> Error in recode_values(y, 2 ~ "20", 4 ~ "40", unmatched = "error") : 
#>   Each location must be matched.
#> ✖ Locations 1, 3, and 5 are unmatched.

# ---------------------------------------------------------------------------
# Lookup tables

# If you were splicing an external lookup vector into `recode()`, you can
# instead use the `from` and `to` arguments of `recode_values()`
x <- c("a", "b", "a", "c", "d", "c")

lookup <- c(
  "a" = "A",
  "b" = "B",
  "c" = "C",
  "d" = "D"
)

recode(x, !!!lookup)
#> [1] "A" "B" "A" "C" "D" "C"
recode_values(x, from = names(lookup), to = unname(lookup))
#> [1] "A" "B" "A" "C" "D" "C"

# `recode_values()` is much more flexible here because the lookup table
# isn't restricted to just character values. We recommend using `tribble()`
# to build your lookup tables.
lookup <- tribble(
  ~from, ~to,
  "a", 1,
  "b", 2,
  "c", 3,
  "d", 4
)

recode_values(x, from = lookup$from, to = lookup$to)
#> [1] 1 2 1 3 4 3

# ---------------------------------------------------------------------------
# Factors

# The factor method of `recode()` can generally be replaced with
# `forcats::fct_recode()`
x <- factor(c("a", "b", "c"))
recode(x, a = "Apple")
#> [1] Apple b     c    
#> Levels: Apple b c
# forcats::fct_recode(x, "Apple" = "a")

# `recode_factor()` does not currently have a direct replacement, but we
# plan to add one to forcats. In the meantime, use a lookup table that
# recodes every case, and then convert the `to` column to a factor. If you
# define your lookup table in your preferred level order, then the conversion
# to factor is straightforward!
y <- c(3, 4, 1, 2, 4, NA)

recode_factor(
  y,
  `1` = "a",
  `2` = "b",
  `3` = "c",
  `4` = "d",
  .missing = "M"
)
#> [1] c d a b d M
#> Levels: a b c d M

lookup <- tribble(
  ~from, ~to,
  1, "a",
  2, "b",
  3, "c",
  4, "d",
  NA, "M"
)
# `factor()` generates levels by sorting the unique values of `to`, which we
# don't want, so we supply `levels = to` directly. Alternatively, use
# `forcats::fct(to)`, which generates levels in order of appearance.
lookup <- mutate(lookup, to = factor(to, levels = to))

recode_values(y, from = lookup$from, to = lookup$to)
#> [1] c d a b d M
#> Levels: a b c d M
```
