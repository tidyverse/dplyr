# Keep or drop columns using their names and types

Select (and optionally rename) variables in a data frame, using a
concise mini-language that makes it easy to refer to variables based on
their name (e.g. `a:f` selects all columns from `a` on the left to `f`
on the right) or type (e.g. `where(is.numeric)` selects all numeric
columns).

### Overview of selection features

Tidyverse selections implement a dialect of R where operators make it
easy to select variables:

- `:` for selecting a range of consecutive variables.

- `!` for taking the complement of a set of variables.

- `&` and `|` for selecting the intersection or the union of two sets of
  variables.

- [`c()`](https://rdrr.io/r/base/c.html) for combining selections.

In addition, you can use **selection helpers**. Some helpers select
specific columns:

- [`everything()`](https://tidyselect.r-lib.org/reference/everything.html):
  Matches all variables.

- [`last_col()`](https://tidyselect.r-lib.org/reference/everything.html):
  Select last variable, possibly with an offset.

- [`group_cols()`](https://dplyr.tidyverse.org/dev/reference/group_cols.md):
  Select all grouping columns.

Other helpers select variables by matching patterns in their names:

- [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Starts with a prefix.

- [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Ends with a suffix.

- [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Contains a literal string.

- [`matches()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a regular expression.

- [`num_range()`](https://tidyselect.r-lib.org/reference/starts_with.html):
  Matches a numerical range like x01, x02, x03.

Or from variables stored in a character vector:

- [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html):
  Matches variable names in a character vector. All names must be
  present, otherwise an out-of-bounds error is thrown.

- [`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html): Same
  as [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  except that no error is thrown for names that don't exist.

Or using a predicate function:

- [`where()`](https://tidyselect.r-lib.org/reference/where.html):
  Applies a function to all variables and selects those for which the
  function returns `TRUE`.

## Usage

``` r
select(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/dev/reference/dplyr_tidy_select.md)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

## Value

An object of the same type as `.data`. The output has the following
properties:

- Rows are not affected.

- Output columns are a subset of input columns, potentially with a
  different order. Columns will be renamed if `new_name = old_name` form
  is used.

- Data frame attributes are preserved.

- Groups are maintained; you can't select off grouping variables.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_lazy`](https://dbplyr.tidyverse.org/reference/select.tbl_lazy.html)),
dplyr (`data.frame`) .

## Examples

Here we show the usage for the basic selection operators. See the
specific help pages to learn about helpers like
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).

The selection language can be used in functions like `dplyr::select()`.
Let's first attach the tidyverse:

    library(tidyverse)

    # For better printing
    iris <- as_tibble(iris)

Select variables by name:

    starwars |> select(height)
    #> # A tibble: 87 x 1
    #>   height
    #>    <int>
    #> 1    172
    #> 2    167
    #> 3     96
    #> 4    202
    #> # i 83 more rows

    iris |> select(Sepal.Length)
    #> # A tibble: 150 x 1
    #>   Sepal.Length
    #>          <dbl>
    #> 1          5.1
    #> 2          4.9
    #> 3          4.7
    #> 4          4.6
    #> # i 146 more rows

Select multiple variables by separating them with commas. Note how the
order of columns is determined by the order of inputs:

    starwars |> select(homeworld, height, mass)
    #> # A tibble: 87 x 3
    #>   homeworld height  mass
    #>   <chr>      <int> <dbl>
    #> 1 Tatooine     172    77
    #> 2 Tatooine     167    75
    #> 3 Naboo         96    32
    #> 4 Tatooine     202   136
    #> # i 83 more rows

    iris |> select(Sepal.Length, Petal.Length)
    #> # A tibble: 150 x 2
    #>   Sepal.Length Petal.Length
    #>          <dbl>        <dbl>
    #> 1          5.1          1.4
    #> 2          4.9          1.4
    #> 3          4.7          1.3
    #> 4          4.6          1.5
    #> # i 146 more rows

If you use a named vector to select columns, the output will have its
columns renamed:

    selection <- c(
      new_homeworld = "homeworld",
      new_height = "height",
      new_mass = "mass"
    )
    starwars |> select(all_of(selection))
    #> # A tibble: 87 x 3
    #>   new_homeworld new_height new_mass
    #>   <chr>              <int>    <dbl>
    #> 1 Tatooine             172       77
    #> 2 Tatooine             167       75
    #> 3 Naboo                 96       32
    #> 4 Tatooine             202      136
    #> # i 83 more rows

### Operators:

The `:` operator selects a range of consecutive variables:

    starwars |> select(name:mass)
    #> # A tibble: 87 x 3
    #>   name           height  mass
    #>   <chr>           <int> <dbl>
    #> 1 Luke Skywalker    172    77
    #> 2 C-3PO             167    75
    #> 3 R2-D2              96    32
    #> 4 Darth Vader       202   136
    #> # i 83 more rows

The `!` operator negates a selection:

    starwars |> select(!(name:mass))
    #> # A tibble: 87 x 11
    #>   hair_color skin_color  eye_color birth_year sex   gender    homeworld species
    #>   <chr>      <chr>       <chr>          <dbl> <chr> <chr>     <chr>     <chr>
    #> 1 blond      fair        blue            19   male  masculine Tatooine  Human
    #> 2 <NA>       gold        yellow         112   none  masculine Tatooine  Droid
    #> 3 <NA>       white, blue red             33   none  masculine Naboo     Droid
    #> 4 none       white       yellow          41.9 male  masculine Tatooine  Human
    #> # i 83 more rows
    #> # i 3 more variables: films <list>, vehicles <list>, starships <list>

    iris |> select(!c(Sepal.Length, Petal.Length))
    #> # A tibble: 150 x 3
    #>   Sepal.Width Petal.Width Species
    #>         <dbl>       <dbl> <fct>
    #> 1         3.5         0.2 setosa
    #> 2         3           0.2 setosa
    #> 3         3.2         0.2 setosa
    #> 4         3.1         0.2 setosa
    #> # i 146 more rows

    iris |> select(!ends_with("Width"))
    #> # A tibble: 150 x 3
    #>   Sepal.Length Petal.Length Species
    #>          <dbl>        <dbl> <fct>
    #> 1          5.1          1.4 setosa
    #> 2          4.9          1.4 setosa
    #> 3          4.7          1.3 setosa
    #> 4          4.6          1.5 setosa
    #> # i 146 more rows

`&` and `|` take the intersection or the union of two selections:

    iris |> select(starts_with("Petal") & ends_with("Width"))
    #> # A tibble: 150 x 1
    #>   Petal.Width
    #>         <dbl>
    #> 1         0.2
    #> 2         0.2
    #> 3         0.2
    #> 4         0.2
    #> # i 146 more rows

    iris |> select(starts_with("Petal") | ends_with("Width"))
    #> # A tibble: 150 x 3
    #>   Petal.Length Petal.Width Sepal.Width
    #>          <dbl>       <dbl>       <dbl>
    #> 1          1.4         0.2         3.5
    #> 2          1.4         0.2         3
    #> 3          1.3         0.2         3.2
    #> 4          1.5         0.2         3.1
    #> # i 146 more rows

To take the difference between two selections, combine the `&` and `!`
operators:

    iris |> select(starts_with("Petal") & !ends_with("Width"))
    #> # A tibble: 150 x 1
    #>   Petal.Length
    #>          <dbl>
    #> 1          1.4
    #> 2          1.4
    #> 3          1.3
    #> 4          1.5
    #> # i 146 more rows

## See also

Other single table verbs:
[`arrange()`](https://dplyr.tidyverse.org/dev/reference/arrange.md),
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)
