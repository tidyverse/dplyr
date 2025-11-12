# Create, modify, and delete columns

`mutate()` creates new columns that are functions of existing variables.
It can also modify (if the name is the same as an existing column) and
delete columns (by setting their value to `NULL`).

## Usage

``` r
mutate(.data, ...)

# S3 method for class 'data.frame'
mutate(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/dev/reference/group_by.md).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/dev/reference/dplyr_by.md).

- .keep:

  Control which columns from `.data` are retained in the output.
  Grouping columns and columns created by `...` are always kept.

  - `"all"` retains all columns from `.data`. This is the default.

  - `"used"` retains only the columns used in `...` to create new
    columns. This is useful for checking your work, as it displays
    inputs and outputs side-by-side.

  - `"unused"` retains only the columns *not* used in `...` to create
    new columns. This is useful if you generate new columns, but no
    longer need the columns used to generate them.

  - `"none"` doesn't retain any extra columns from `.data`. Only the
    grouping variables and columns created by `...` are kept.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  Optionally, control where new columns should appear (the default is to
  add to the right hand side). See
  [`relocate()`](https://dplyr.tidyverse.org/dev/reference/relocate.md)
  for more details.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Columns from `.data` will be preserved according to the `.keep`
  argument.

- Existing columns that are modified by `...` will always be returned in
  their original location.

- New columns created through `...` will be placed according to the
  `.before` and `.after` arguments.

- The number of rows is not affected.

- Columns given the value `NULL` will be removed.

- Groups will be recomputed if a grouping variable is mutated.

- Data frame attributes are preserved.

## Useful mutate functions

- [`+`](https://rdrr.io/r/base/Arithmetic.html),
  [`-`](https://rdrr.io/r/base/Arithmetic.html),
  [`log()`](https://rdrr.io/r/base/Log.html), etc., for their usual
  mathematical meanings

- [`lead()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md),
  [`lag()`](https://dplyr.tidyverse.org/dev/reference/lead-lag.md)

- [`dense_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`min_rank()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`percent_rank()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md),
  [`row_number()`](https://dplyr.tidyverse.org/dev/reference/row_number.md),
  [`cume_dist()`](https://dplyr.tidyverse.org/dev/reference/percent_rank.md),
  [`ntile()`](https://dplyr.tidyverse.org/dev/reference/ntile.md)

- [`cumsum()`](https://rdrr.io/r/base/cumsum.html),
  [`cummean()`](https://dplyr.tidyverse.org/dev/reference/cumall.md),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html),
  [`cummax()`](https://rdrr.io/r/base/cumsum.html),
  [`cumany()`](https://dplyr.tidyverse.org/dev/reference/cumall.md),
  [`cumall()`](https://dplyr.tidyverse.org/dev/reference/cumall.md)

- [`na_if()`](https://dplyr.tidyverse.org/dev/reference/na_if.md),
  [`coalesce()`](https://dplyr.tidyverse.org/dev/reference/coalesce.md)

- [`if_else()`](https://dplyr.tidyverse.org/dev/reference/if_else.md),
  [`recode()`](https://dplyr.tidyverse.org/dev/reference/recode.md),
  [`case_when()`](https://dplyr.tidyverse.org/dev/reference/case-and-replace-when.md)

## Grouped tibbles

Because mutating expressions are computed within groups, they may yield
different results on grouped tibbles. This will be the case as soon as
an aggregating, lagging, or ranking function is involved. Compare this
ungrouped mutate:

    starwars |>
      select(name, mass, species) |>
      mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

With the grouped equivalent:

    starwars |>
      select(name, mass, species) |>
      group_by(species) |>
      mutate(mass_norm = mass / mean(mass, na.rm = TRUE))

The former normalises `mass` by the global average whereas the latter
normalises by the averages within species levels.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

Methods available in currently loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/mutate.tbl_lazy.html)),
dplyr (`data.frame`) .

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)

## Examples

``` r
# Newly created variables are available immediately
starwars |>
  select(name, mass) |>
  mutate(
    mass2 = mass * 2,
    mass2_squared = mass2 * mass2
  )
#> # A tibble: 87 × 4
#>    name                mass mass2 mass2_squared
#>    <chr>              <dbl> <dbl>         <dbl>
#>  1 Luke Skywalker        77   154         23716
#>  2 C-3PO                 75   150         22500
#>  3 R2-D2                 32    64          4096
#>  4 Darth Vader          136   272         73984
#>  5 Leia Organa           49    98          9604
#>  6 Owen Lars            120   240         57600
#>  7 Beru Whitesun Lars    75   150         22500
#>  8 R5-D4                 32    64          4096
#>  9 Biggs Darklighter     84   168         28224
#> 10 Obi-Wan Kenobi        77   154         23716
#> # ℹ 77 more rows

# As well as adding new variables, you can use mutate() to
# remove variables and modify existing variables.
starwars |>
  select(name, height, mass, homeworld) |>
  mutate(
    mass = NULL,
    height = height * 0.0328084 # convert to feet
  )
#> # A tibble: 87 × 3
#>    name               height homeworld
#>    <chr>               <dbl> <chr>    
#>  1 Luke Skywalker       5.64 Tatooine 
#>  2 C-3PO                5.48 Tatooine 
#>  3 R2-D2                3.15 Naboo    
#>  4 Darth Vader          6.63 Tatooine 
#>  5 Leia Organa          4.92 Alderaan 
#>  6 Owen Lars            5.84 Tatooine 
#>  7 Beru Whitesun Lars   5.41 Tatooine 
#>  8 R5-D4                3.18 Tatooine 
#>  9 Biggs Darklighter    6.00 Tatooine 
#> 10 Obi-Wan Kenobi       5.97 Stewjon  
#> # ℹ 77 more rows

# Use across() with mutate() to apply a transformation
# to multiple columns in a tibble.
starwars |>
  select(name, homeworld, species) |>
  mutate(across(!name, as.factor))
#> # A tibble: 87 × 3
#>    name               homeworld species
#>    <chr>              <fct>     <fct>  
#>  1 Luke Skywalker     Tatooine  Human  
#>  2 C-3PO              Tatooine  Droid  
#>  3 R2-D2              Naboo     Droid  
#>  4 Darth Vader        Tatooine  Human  
#>  5 Leia Organa        Alderaan  Human  
#>  6 Owen Lars          Tatooine  Human  
#>  7 Beru Whitesun Lars Tatooine  Human  
#>  8 R5-D4              Tatooine  Droid  
#>  9 Biggs Darklighter  Tatooine  Human  
#> 10 Obi-Wan Kenobi     Stewjon   Human  
#> # ℹ 77 more rows
# see more in ?across

# Window functions are useful for grouped mutates:
starwars |>
  select(name, mass, homeworld) |>
  group_by(homeworld) |>
  mutate(rank = min_rank(desc(mass)))
#> # A tibble: 87 × 4
#> # Groups:   homeworld [49]
#>    name                mass homeworld  rank
#>    <chr>              <dbl> <chr>     <int>
#>  1 Luke Skywalker        77 Tatooine      5
#>  2 C-3PO                 75 Tatooine      6
#>  3 R2-D2                 32 Naboo         6
#>  4 Darth Vader          136 Tatooine      1
#>  5 Leia Organa           49 Alderaan      2
#>  6 Owen Lars            120 Tatooine      2
#>  7 Beru Whitesun Lars    75 Tatooine      6
#>  8 R5-D4                 32 Tatooine      8
#>  9 Biggs Darklighter     84 Tatooine      3
#> 10 Obi-Wan Kenobi        77 Stewjon       1
#> # ℹ 77 more rows
# see `vignette("window-functions")` for more details

# By default, new columns are placed on the far right.
df <- tibble(x = 1, y = 2)
df |> mutate(z = x + y)
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
df |> mutate(z = x + y, .before = 1)
#> # A tibble: 1 × 3
#>       z     x     y
#>   <dbl> <dbl> <dbl>
#> 1     3     1     2
df |> mutate(z = x + y, .after = x)
#> # A tibble: 1 × 3
#>       x     z     y
#>   <dbl> <dbl> <dbl>
#> 1     1     3     2

# By default, mutate() keeps all columns from the input data.
df <- tibble(x = 1, y = 2, a = "a", b = "b")
df |> mutate(z = x + y, .keep = "all") # the default
#> # A tibble: 1 × 5
#>       x     y a     b         z
#>   <dbl> <dbl> <chr> <chr> <dbl>
#> 1     1     2 a     b         3
df |> mutate(z = x + y, .keep = "used")
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     1     2     3
df |> mutate(z = x + y, .keep = "unused")
#> # A tibble: 1 × 3
#>   a     b         z
#>   <chr> <chr> <dbl>
#> 1 a     b         3
df |> mutate(z = x + y, .keep = "none")
#> # A tibble: 1 × 1
#>       z
#>   <dbl>
#> 1     3

# Grouping ----------------------------------------
# The mutate operation may yield different results on grouped
# tibbles because the expressions are computed within groups.
# The following normalises `mass` by the global average:
starwars |>
  select(name, mass, species) |>
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#> # A tibble: 87 × 4
#>    name                mass species mass_norm
#>    <chr>              <dbl> <chr>       <dbl>
#>  1 Luke Skywalker        77 Human       0.791
#>  2 C-3PO                 75 Droid       0.771
#>  3 R2-D2                 32 Droid       0.329
#>  4 Darth Vader          136 Human       1.40 
#>  5 Leia Organa           49 Human       0.504
#>  6 Owen Lars            120 Human       1.23 
#>  7 Beru Whitesun Lars    75 Human       0.771
#>  8 R5-D4                 32 Droid       0.329
#>  9 Biggs Darklighter     84 Human       0.863
#> 10 Obi-Wan Kenobi        77 Human       0.791
#> # ℹ 77 more rows

# Whereas this normalises `mass` by the averages within species
# levels:
starwars |>
  select(name, mass, species) |>
  group_by(species) |>
  mutate(mass_norm = mass / mean(mass, na.rm = TRUE))
#> # A tibble: 87 × 4
#> # Groups:   species [38]
#>    name                mass species mass_norm
#>    <chr>              <dbl> <chr>       <dbl>
#>  1 Luke Skywalker        77 Human       0.947
#>  2 C-3PO                 75 Droid       1.08 
#>  3 R2-D2                 32 Droid       0.459
#>  4 Darth Vader          136 Human       1.67 
#>  5 Leia Organa           49 Human       0.603
#>  6 Owen Lars            120 Human       1.48 
#>  7 Beru Whitesun Lars    75 Human       0.922
#>  8 R5-D4                 32 Droid       0.459
#>  9 Biggs Darklighter     84 Human       1.03 
#> 10 Obi-Wan Kenobi        77 Human       0.947
#> # ℹ 77 more rows

# Indirection ----------------------------------------
# Refer to column names stored as strings with the `.data` pronoun:
vars <- c("mass", "height")
mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#> # A tibble: 87 × 15
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
#> # ℹ 7 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>, prod <dbl>
# Learn more in ?rlang::args_data_masking
```
