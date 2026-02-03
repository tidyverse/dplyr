# Starwars characters

The original data, from SWAPI, the Star Wars API,
<https://swapi.py4e.com/>, has been revised to reflect additional
research into gender and sex determinations of characters.

## Usage

``` r
starwars
```

## Format

A tibble with 87 rows and 14 variables:

- name:

  Name of the character

- height:

  Height (cm)

- mass:

  Weight (kg)

- hair_color,skin_color,eye_color:

  Hair, skin, and eye colors

- birth_year:

  Year born (BBY = Before Battle of Yavin)

- sex:

  The biological sex of the character, namely male, female,
  hermaphroditic, or none (as in the case for Droids).

- gender:

  The gender role or gender identity of the character as determined by
  their personality or the way they were programmed (as in the case for
  Droids).

- homeworld:

  Name of homeworld

- species:

  Name of species

- films:

  List of films the character appeared in

- vehicles:

  List of vehicles the character has piloted

- starships:

  List of starships the character has piloted

## Examples

``` r
starwars
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
