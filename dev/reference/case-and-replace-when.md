# A general vectorised if-else

`case_when()` and `replace_when()` are two forms of vectorized
[`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md).
They work by evaluating each case sequentially and using the first match
for each element to determine the corresponding value in the output
vector.

- Use `case_when()` when creating an entirely new vector.

- Use `replace_when()` when partially updating an existing vector.

If you are just replacing a few values within an existing vector, then
`replace_when()` is always a better choice because it is type stable,
size stable, pipes better, and better expresses intent.

A major difference between the two functions is what happens when no
cases match:

- `case_when()` falls through to a `.default` as a final "else"
  statement.

- `replace_when()` retains the original values from `x`.

## Usage

``` r
case_when(
  ...,
  .default = NULL,
  .unmatched = "default",
  .ptype = NULL,
  .size = NULL
)

replace_when(x, ...)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas. The left hand side (LHS) determines
  which values match this case. The right hand side (RHS) provides the
  replacement value.

  For `case_when()`:

  - The LHS inputs must be logical vectors. For backwards compatibility,
    scalars are
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html),
    but we no longer recommend supplying scalars.

  - The RHS inputs will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to their common type, and will be
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
    to the common size of the LHS inputs.

  For `replace_when()`:

  - The LHS inputs must be logical vectors the same size as `x`.

  - The RHS inputs will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.html)
    to the type of `x` and
    [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
    to the size of `x`.

  `NULL` inputs are ignored.

- .default:

  The value used when all of the LHS inputs return either `FALSE` or
  `NA`.

  - If `NULL`, the default, a missing value will be used.

  - If provided, `.default` will follow the same type and size rules as
    the RHS inputs.

  `NA` values in the LHS conditions are treated like `FALSE`, meaning
  that the result at those locations will be assigned the `.default`
  value. To handle missing values in the conditions differently, you
  must explicitly catch them with another condition before they fall
  through to the `.default`. This typically involves some variation of
  `is.na(x) ~ value` tailored to your usage of `case_when()`.

- .unmatched:

  Handling of unmatched locations.

  One of:

  - `"default"` to use `.default` in unmatched locations.

  - `"error"` to error when there are unmatched locations.

- .ptype:

  An optional prototype declaring the desired output type. If supplied,
  this overrides the common type of the RHS inputs.

- .size:

  An optional size declaring the desired output size. If supplied, this
  overrides the common size computed from the LHS inputs.

- x:

  A vector.

## Value

For `case_when()`, a new vector where the size is the common size of the
LHS inputs, the type is the common type of the RHS inputs, and the names
correspond to the names of the RHS elements used in the result.

For `replace_when()`, an updated version of `x`, with the same size,
type, and names as `x`.

## See also

[`recode_values()`](https://dplyr.tidyverse.org/dev/reference/recode-and-replace-values.md),
[`vctrs::vec_case_when()`](https://vctrs.r-lib.org/reference/vec-case-and-replace.html)

## Examples

``` r
x <- 1:70
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  .default = as.character(x)
)
#>  [1] "1"         "2"         "3"         "4"         "fizz"     
#>  [6] "6"         "buzz"      "8"         "9"         "fizz"     
#> [11] "11"        "12"        "13"        "buzz"      "fizz"     
#> [16] "16"        "17"        "18"        "19"        "fizz"     
#> [21] "buzz"      "22"        "23"        "24"        "fizz"     
#> [26] "26"        "27"        "buzz"      "29"        "fizz"     
#> [31] "31"        "32"        "33"        "34"        "fizz buzz"
#> [36] "36"        "37"        "38"        "39"        "fizz"     
#> [41] "41"        "buzz"      "43"        "44"        "fizz"     
#> [46] "46"        "47"        "48"        "buzz"      "fizz"     
#> [51] "51"        "52"        "53"        "54"        "fizz"     
#> [56] "buzz"      "57"        "58"        "59"        "fizz"     
#> [61] "61"        "62"        "buzz"      "64"        "fizz"     
#> [66] "66"        "67"        "68"        "69"        "fizz buzz"

# Like an if statement, the arguments are evaluated in order, so you must
# proceed from the most specific to the most general. This won't work:
case_when(
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  x %% 35 == 0 ~ "fizz buzz",
  .default = as.character(x)
)
#>  [1] "1"    "2"    "3"    "4"    "fizz" "6"    "buzz" "8"    "9"   
#> [10] "fizz" "11"   "12"   "13"   "buzz" "fizz" "16"   "17"   "18"  
#> [19] "19"   "fizz" "buzz" "22"   "23"   "24"   "fizz" "26"   "27"  
#> [28] "buzz" "29"   "fizz" "31"   "32"   "33"   "34"   "fizz" "36"  
#> [37] "37"   "38"   "39"   "fizz" "41"   "buzz" "43"   "44"   "fizz"
#> [46] "46"   "47"   "48"   "buzz" "fizz" "51"   "52"   "53"   "54"  
#> [55] "fizz" "buzz" "57"   "58"   "59"   "fizz" "61"   "62"   "buzz"
#> [64] "64"   "fizz" "66"   "67"   "68"   "69"   "fizz"

# If none of the cases match and no `.default` is supplied, NA is used:
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz"
)
#>  [1] NA          NA          NA          NA          "fizz"     
#>  [6] NA          "buzz"      NA          NA          "fizz"     
#> [11] NA          NA          NA          "buzz"      "fizz"     
#> [16] NA          NA          NA          NA          "fizz"     
#> [21] "buzz"      NA          NA          NA          "fizz"     
#> [26] NA          NA          "buzz"      NA          "fizz"     
#> [31] NA          NA          NA          NA          "fizz buzz"
#> [36] NA          NA          NA          NA          "fizz"     
#> [41] NA          "buzz"      NA          NA          "fizz"     
#> [46] NA          NA          NA          "buzz"      "fizz"     
#> [51] NA          NA          NA          NA          "fizz"     
#> [56] "buzz"      NA          NA          NA          "fizz"     
#> [61] NA          NA          "buzz"      NA          "fizz"     
#> [66] NA          NA          NA          NA          "fizz buzz"

# Note that `NA` values on the LHS are treated like `FALSE` and will be
# assigned the `.default` value. You must handle them explicitly if you
# want to use a different value. The exact way to handle missing values is
# dependent on the set of LHS conditions you use.
x[2:4] <- NA_real_
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "nope",
  .default = as.character(x)
)
#>  [1] "1"         "nope"      "nope"      "nope"      "fizz"     
#>  [6] "6"         "buzz"      "8"         "9"         "fizz"     
#> [11] "11"        "12"        "13"        "buzz"      "fizz"     
#> [16] "16"        "17"        "18"        "19"        "fizz"     
#> [21] "buzz"      "22"        "23"        "24"        "fizz"     
#> [26] "26"        "27"        "buzz"      "29"        "fizz"     
#> [31] "31"        "32"        "33"        "34"        "fizz buzz"
#> [36] "36"        "37"        "38"        "39"        "fizz"     
#> [41] "41"        "buzz"      "43"        "44"        "fizz"     
#> [46] "46"        "47"        "48"        "buzz"      "fizz"     
#> [51] "51"        "52"        "53"        "54"        "fizz"     
#> [56] "buzz"      "57"        "58"        "59"        "fizz"     
#> [61] "61"        "62"        "buzz"      "64"        "fizz"     
#> [66] "66"        "67"        "68"        "69"        "fizz buzz"

# `case_when()` is not a replacement for basic if/else control flow. When
# you have a single scalar condition, using if/else is faster, simpler to
# reason about, and is lazy on the branch that isn't run. For example, this
# seems to work:
x <- "value"
case_when(is.character(x) ~ x, .default = "not-a-character")
#> [1] "value"

# Until `x` is a non-character type
x <- 1
try(case_when(is.character(x) ~ x, .default = "not-a-character"))
#> Error in case_when(is.character(x) ~ x, .default = "not-a-character") : 
#>   Can't combine <double> and `.default` <character>.

# Instead, you should use if/else
if (is.character(x)) {
  y <- x
} else {
  y <- "not-a-character"
}
y
#> [1] "not-a-character"

# If you believe that you've covered every possible case, then set
# `.unmatched = "error"` rather than supplying a `.default`. This adds an
# extra layer of safety to `case_when()` and is particularly useful when you
# have a series of complex expressions!
set.seed(123)
x <- sample(50)

# Oops, we forgot to handle `50`
try(case_when(
  x < 10 ~ "ten",
  x < 20 ~ "twenty",
  x < 30 ~ "thirty",
  x < 40 ~ "forty",
  x < 50 ~ "fifty",
  .unmatched = "error"
))
#> Error in case_when(x < 10 ~ "ten", x < 20 ~ "twenty", x < 30 ~ "thirty",  : 
#>   Each location must be matched.
#> ✖ Location 31 is unmatched.

case_when(
  x < 10 ~ "ten",
  x < 20 ~ "twenty",
  x < 30 ~ "thirty",
  x < 40 ~ "forty",
  x <= 50 ~ "fifty",
  .unmatched = "error"
)
#>  [1] "forty"  "twenty" "twenty" "ten"    "fifty"  "fifty"  "forty" 
#>  [8] "fifty"  "thirty" "thirty" "thirty" "ten"    "fifty"  "thirty"
#> [15] "ten"    "thirty" "ten"    "fifty"  "ten"    "twenty" "forty" 
#> [22] "twenty" "ten"    "fifty"  "twenty" "twenty" "forty"  "thirty"
#> [29] "twenty" "fifty"  "fifty"  "twenty" "thirty" "forty"  "forty" 
#> [36] "thirty" "twenty" "fifty"  "thirty" "forty"  "forty"  "forty" 
#> [43] "fifty"  "ten"    "ten"    "ten"    "thirty" "fifty"  "twenty"
#> [50] "forty" 

# Note that `NA` is considered unmatched and must be handled with its own
# explicit case, even if that case just propagates the missing value!
x[c(2, 5)] <- NA

case_when(
  x < 10 ~ "ten",
  x < 20 ~ "twenty",
  x < 30 ~ "thirty",
  x < 40 ~ "forty",
  x <= 50 ~ "fifty",
  is.na(x) ~ NA,
  .unmatched = "error"
)
#>  [1] "forty"  NA       "twenty" "ten"    NA       "fifty"  "forty" 
#>  [8] "fifty"  "thirty" "thirty" "thirty" "ten"    "fifty"  "thirty"
#> [15] "ten"    "thirty" "ten"    "fifty"  "ten"    "twenty" "forty" 
#> [22] "twenty" "ten"    "fifty"  "twenty" "twenty" "forty"  "thirty"
#> [29] "twenty" "fifty"  "fifty"  "twenty" "thirty" "forty"  "forty" 
#> [36] "thirty" "twenty" "fifty"  "thirty" "forty"  "forty"  "forty" 
#> [43] "fifty"  "ten"    "ten"    "ten"    "thirty" "fifty"  "twenty"
#> [50] "forty" 

# `replace_when()` is useful when you're updating an existing vector,
# rather than creating an entirely new one. Note the so-far unused "puppy"
# factor level:
pets <- tibble(
  name = c("Max", "Bella", "Chuck", "Luna", "Cooper"),
  type = factor(
    c("dog", "dog", "cat", "dog", "cat"),
    levels = c("dog", "cat", "puppy")
  ),
  age = c(1, 3, 5, 2, 4)
)

# We can replace some values with `"puppy"` based on arbitrary conditions.
# Even though we are using a character `"puppy"` value, `replace_when()` will
# automatically cast it to the factor type of `type` for us.
pets |>
  mutate(
    type = replace_when(type, type == "dog" & age <= 2 ~ "puppy")
  )
#> # A tibble: 5 × 3
#>   name   type    age
#>   <chr>  <fct> <dbl>
#> 1 Max    puppy     1
#> 2 Bella  dog       3
#> 3 Chuck  cat       5
#> 4 Luna   puppy     2
#> 5 Cooper cat       4

# Compare that with this `case_when()` call, which loses the factor class.
# It's always better to use `replace_when()` when updating a few values in
# an existing vector!
pets |>
  mutate(
    type = case_when(type == "dog" & age <= 2 ~ "puppy", .default = type)
  )
#> # A tibble: 5 × 3
#>   name   type    age
#>   <chr>  <chr> <dbl>
#> 1 Max    puppy     1
#> 2 Bella  dog       3
#> 3 Chuck  cat       5
#> 4 Luna   puppy     2
#> 5 Cooper cat       4

# `case_when()` and `replace_when()` evaluate all RHS expressions, and then
# construct their result by extracting the selected (via the LHS expressions)
# parts. For example, `NaN`s are produced here because `sqrt(y)` is evaluated
# on all of `y`, not just where `y >= 0`.
y <- seq(-2, 2, by = .5)
replace_when(y, y >= 0 ~ sqrt(y))
#> Warning: NaNs produced
#> [1] -2.0000000 -1.5000000 -1.0000000 -0.5000000  0.0000000  0.7071068
#> [7]  1.0000000  1.2247449  1.4142136

# These functions are particularly useful inside `mutate()` when you want to
# create a new variable that relies on a complex combination of existing
# variables
starwars |>
  select(name:mass, gender, species) |>
  mutate(
    type = case_when(
      height > 200 | mass > 200 ~ "large",
      species == "Droid" ~ "robot",
      .default = "other"
    )
  )
#> # A tibble: 87 × 6
#>    name               height  mass gender    species type 
#>    <chr>               <int> <dbl> <chr>     <chr>   <chr>
#>  1 Luke Skywalker        172    77 masculine Human   other
#>  2 C-3PO                 167    75 masculine Droid   robot
#>  3 R2-D2                  96    32 masculine Droid   robot
#>  4 Darth Vader           202   136 masculine Human   large
#>  5 Leia Organa           150    49 feminine  Human   other
#>  6 Owen Lars             178   120 masculine Human   other
#>  7 Beru Whitesun Lars    165    75 feminine  Human   other
#>  8 R5-D4                  97    32 masculine Droid   robot
#>  9 Biggs Darklighter     183    84 masculine Human   other
#> 10 Obi-Wan Kenobi        182    77 masculine Human   other
#> # ℹ 77 more rows

# `case_when()` is not a tidy eval function. If you'd like to reuse
# the same patterns, extract the `case_when()` call into a normal
# function:
case_character_type <- function(height, mass, species) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    species == "Droid" ~ "robot",
    .default = "other"
  )
}

case_character_type(150, 250, "Droid")
#> [1] "large"
case_character_type(150, 150, "Droid")
#> [1] "robot"

# Such functions can be used inside `mutate()` as well:
starwars |>
  mutate(type = case_character_type(height, mass, species)) |>
  pull(type)
#>  [1] "other" "robot" "robot" "large" "other" "other" "other" "robot"
#>  [9] "other" "other" "other" "other" "large" "other" "other" "large"
#> [17] "other" "other" "other" "other" "other" "robot" "other" "other"
#> [25] "other" "other" "other" "other" "other" "other" "other" "other"
#> [33] "other" "other" "other" "large" "large" "other" "other" "other"
#> [41] "other" "other" "other" "other" "other" "other" "other" "other"
#> [49] "other" "other" "other" "other" "other" "other" "other" "large"
#> [57] "other" "other" "other" "other" "other" "other" "other" "other"
#> [65] "other" "other" "other" "other" "other" "other" "large" "large"
#> [73] "other" "robot" "other" "other" "other" "large" "large" "other"
#> [81] "other" "large" "other" "other" "other" "robot" "other"

# `case_when()` ignores `NULL` inputs. This is useful when you'd
# like to use a pattern only under certain conditions. Here we'll
# take advantage of the fact that `if` returns `NULL` when there is
# no `else` clause:
case_character_type <- function(height, mass, species, robots = TRUE) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    if (robots) species == "Droid" ~ "robot",
    .default = "other"
  )
}

starwars |>
  mutate(type = case_character_type(height, mass, species, robots = FALSE)) |>
  pull(type)
#>  [1] "other" "other" "other" "large" "other" "other" "other" "other"
#>  [9] "other" "other" "other" "other" "large" "other" "other" "large"
#> [17] "other" "other" "other" "other" "other" "other" "other" "other"
#> [25] "other" "other" "other" "other" "other" "other" "other" "other"
#> [33] "other" "other" "other" "large" "large" "other" "other" "other"
#> [41] "other" "other" "other" "other" "other" "other" "other" "other"
#> [49] "other" "other" "other" "other" "other" "other" "other" "large"
#> [57] "other" "other" "other" "other" "other" "other" "other" "other"
#> [65] "other" "other" "other" "other" "other" "other" "large" "large"
#> [73] "other" "other" "other" "other" "other" "large" "large" "other"
#> [81] "other" "large" "other" "other" "other" "other" "other"

# `replace_when()` can also be used in combination with `pick()` to
# conditionally mutate rows within multiple columns using a single condition.
# Here `replace_when()` returns a data frame with new `species` and `name`
# columns, which `mutate()` then automatically unpacks.
starwars |>
  select(homeworld, species, name) |>
  mutate(replace_when(
    pick(species, name),
    homeworld == "Tatooine" ~ tibble(
      species = "Tatooinese",
      name = paste(name, "(Tatooine)")
    )
  ))
#> # A tibble: 87 × 3
#>    homeworld species    name                         
#>    <chr>     <chr>      <chr>                        
#>  1 Tatooine  Tatooinese Luke Skywalker (Tatooine)    
#>  2 Tatooine  Tatooinese C-3PO (Tatooine)             
#>  3 Naboo     Droid      R2-D2                        
#>  4 Tatooine  Tatooinese Darth Vader (Tatooine)       
#>  5 Alderaan  Human      Leia Organa                  
#>  6 Tatooine  Tatooinese Owen Lars (Tatooine)         
#>  7 Tatooine  Tatooinese Beru Whitesun Lars (Tatooine)
#>  8 Tatooine  Tatooinese R5-D4 (Tatooine)             
#>  9 Tatooine  Tatooinese Biggs Darklighter (Tatooine) 
#> 10 Stewjon   Human      Obi-Wan Kenobi               
#> # ℹ 77 more rows
```
