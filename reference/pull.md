# Extract a single column

`pull()` is similar to `$`. It's mostly useful because it looks a little
nicer in pipes, it also works with remote data frames, and it can
optionally name the output.

## Usage

``` r
pull(.data, var = -1, name = NULL, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- var:

  A variable specified as:

  - a literal variable name

  - a positive integer, giving the position counting from the left

  - a negative integer, giving the position counting from the right.

  The default returns the last column (on the assumption that's the
  column you've created most recently).

  This argument is taken by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names and column locations).

- name:

  An optional parameter that specifies the column to be used as names
  for a named vector. Specified in a similar manner as `var`.

- ...:

  For use by methods.

## Value

A vector the same size as `.data`.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`tbl_sql`](https://dbplyr.tidyverse.org/reference/pull.tbl_sql.html)),
dplyr (`data.frame`) .

## Examples

``` r
mtcars |> pull(-1)
#>  [1] 4 4 1 1 2 1 4 2 2 4 4 3 3 3 4 4 4 1 2 1 1 2 2 4 2 1 2 2 4 6 8 2
mtcars |> pull(1)
#>  [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3
#> [14] 15.2 10.4 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3
#> [27] 26.0 30.4 15.8 19.7 15.0 21.4
mtcars |> pull(cyl)
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4

# Also works for remote sources
df <- dbplyr::memdb_frame(x = 1:10, y = 10:1, .name = "pull-ex")
df |>
  mutate(z = x * y) |>
  pull()
#>  [1] 10 18 24 28 30 30 28 24 18 10

# Pull a named vector
starwars |> pull(height, name)
#>        Luke Skywalker                 C-3PO                 R2-D2 
#>                   172                   167                    96 
#>           Darth Vader           Leia Organa             Owen Lars 
#>                   202                   150                   178 
#>    Beru Whitesun Lars                 R5-D4     Biggs Darklighter 
#>                   165                    97                   183 
#>        Obi-Wan Kenobi      Anakin Skywalker        Wilhuff Tarkin 
#>                   182                   188                   180 
#>             Chewbacca              Han Solo                Greedo 
#>                   228                   180                   173 
#> Jabba Desilijic Tiure        Wedge Antilles      Jek Tono Porkins 
#>                   175                   170                   180 
#>                  Yoda             Palpatine             Boba Fett 
#>                    66                   170                   183 
#>                 IG-88                 Bossk      Lando Calrissian 
#>                   200                   190                   177 
#>                 Lobot                Ackbar            Mon Mothma 
#>                   175                   180                   150 
#>          Arvel Crynyd Wicket Systri Warrick             Nien Nunb 
#>                    NA                    88                   160 
#>          Qui-Gon Jinn           Nute Gunray         Finis Valorum 
#>                   193                   191                   170 
#>         Padmé Amidala         Jar Jar Binks          Roos Tarpals 
#>                   185                   196                   224 
#>            Rugor Nass              Ric Olié                 Watto 
#>                   206                   183                   137 
#>               Sebulba         Quarsh Panaka        Shmi Skywalker 
#>                   112                   183                   163 
#>            Darth Maul           Bib Fortuna           Ayla Secura 
#>                   175                   180                   178 
#>          Ratts Tyerel              Dud Bolt               Gasgano 
#>                    79                    94                   122 
#>        Ben Quadinaros            Mace Windu          Ki-Adi-Mundi 
#>                   163                   188                   198 
#>             Kit Fisto             Eeth Koth            Adi Gallia 
#>                   196                   171                   184 
#>           Saesee Tiin           Yarael Poof              Plo Koon 
#>                   188                   264                   188 
#>            Mas Amedda          Gregar Typho                 Cordé 
#>                   196                   185                   157 
#>           Cliegg Lars     Poggle the Lesser       Luminara Unduli 
#>                   183                   183                   170 
#>         Barriss Offee                 Dormé                 Dooku 
#>                   166                   165                   193 
#>   Bail Prestor Organa            Jango Fett            Zam Wesell 
#>                   191                   183                   168 
#>       Dexter Jettster               Lama Su               Taun We 
#>                   198                   229                   213 
#>            Jocasta Nu                R4-P17            Wat Tambor 
#>                   167                    96                   193 
#>              San Hill              Shaak Ti              Grievous 
#>                   191                   178                   216 
#>               Tarfful       Raymus Antilles             Sly Moore 
#>                   234                   188                   178 
#>            Tion Medon                  Finn                   Rey 
#>                   206                    NA                    NA 
#>           Poe Dameron                   BB8        Captain Phasma 
#>                    NA                    NA                    NA 
```
