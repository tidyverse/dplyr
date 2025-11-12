# Convert values to `NA`

This is a translation of the SQL command `NULLIF`. It is useful if you
want to convert an annoying value to `NA`.

## Usage

``` r
na_if(x, y)
```

## Arguments

- x:

  Vector to modify

- y:

  Value or vector to compare against. When `x` and `y` are equal, the
  value in `x` will be replaced with `NA`.

  `y` is
  [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html) to
  the type of `x` before comparison.

  `y` is
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `x` before comparison. This means that `y` can be a
  vector with the same size as `x`, but most of the time this will be a
  single value.

## Value

A modified version of `x` that replaces any values that are equal to `y`
with `NA`.

## See also

- [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)
  to replace `NA`s with the first non-missing value.

- [`replace_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md)
  for making arbitrary replacements by value.

- [`replace_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)
  for making arbitrary replacements using logical conditions.

## Examples

``` r
# `na_if()` is useful for replacing a single problematic value with `NA`
na_if(c(-99, 1, 4, 3, -99, 5), -99)
#> [1] NA  1  4  3 NA  5
na_if(c("abc", "def", "", "ghi"), "")
#> [1] "abc" "def" NA    "ghi"

# You can use it to standardize `NaN`s to `NA`
na_if(c(1, NaN, NA, 2, NaN), NaN)
#> [1]  1 NA NA  2 NA

# Because `na_if()` is an R translation of SQL's `NULLIF` command,
# it compares `x` and `y` element by element. Where `x` and `y` are
# equal, the value in `x` is replaced with an `NA`.
na_if(
  x = c(1, 2, 5, 5, 6),
  y = c(0, 2, 3, 5, 4)
)
#> [1]  1 NA  5 NA  6

# If you have multiple problematic values that you'd like to replace with
# `NA`, then `replace_values()` is a better choice than `na_if()`
x <- c(-99, 1, 4, 0, -99, 5, -1, 0, 5)
replace_values(x, c(0, -1, -99) ~ NA)
#> [1] NA  1  4 NA NA  5 NA NA  5

# You'd have to nest `na_if()`s to achieve this
try(na_if(x, c(0, -1, -99)))
#> Error in na_if(x, c(0, -1, -99)) : Can't recycle `y` (size 3) to size 9.
na_if(na_if(na_if(x, 0), -1), -99)
#> [1] NA  1  4 NA NA  5 NA NA  5

# If you'd like to replace values that match a logical condition with `NA`,
# use `replace_when()`
replace_when(x, x < 0 ~ NA)
#> [1] NA  1  4  0 NA  5 NA  0  5

# If you'd like to replace `NA` with some other value, use `replace_values()`
x <- c(NA, 5, 2, NA, 0, 3)
replace_values(x, NA ~ 0)
#> [1] 0 5 2 0 0 3

# `na_if()` is particularly useful inside `mutate()`
starwars |>
  select(name, eye_color) |>
  mutate(eye_color = na_if(eye_color, "unknown"))
#> # A tibble: 87 × 2
#>    name               eye_color
#>    <chr>              <chr>    
#>  1 Luke Skywalker     blue     
#>  2 C-3PO              yellow   
#>  3 R2-D2              red      
#>  4 Darth Vader        yellow   
#>  5 Leia Organa        brown    
#>  6 Owen Lars          blue     
#>  7 Beru Whitesun Lars blue     
#>  8 R5-D4              red      
#>  9 Biggs Darklighter  brown    
#> 10 Obi-Wan Kenobi     blue-gray
#> # ℹ 77 more rows

# `na_if()` can also be used with `mutate()` and `across()`
# to alter multiple columns
starwars |>
   mutate(across(where(is.character), ~na_if(., "unknown")))
#> # A tibble: 87 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Luke …    172    77 blond      fair       blue            19   male 
#>  2 C-3PO     167    75 NA         gold       yellow         112   none 
#>  3 R2-D2      96    32 NA         white, bl… red             33   none 
#>  4 Darth…    202   136 none       white      yellow          41.9 male 
#>  5 Leia …    150    49 brown      light      brown           19   fema…
#>  6 Owen …    178   120 brown, gr… light      blue            52   male 
#>  7 Beru …    165    75 brown      light      blue            47   fema…
#>  8 R5-D4      97    32 NA         white, red red             NA   none 
#>  9 Biggs…    183    84 black      light      brown           24   male 
#> 10 Obi-W…    182    77 auburn, w… fair       blue-gray       57   male 
#> # ℹ 77 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
```
