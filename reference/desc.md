# Descending order

Transform a vector into a format that will be sorted in descending
order. This is useful within
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.md).

## Usage

``` r
desc(x)
```

## Arguments

- x:

  vector to transform

## Examples

``` r
desc(1:10)
#>  [1]  -1  -2  -3  -4  -5  -6  -7  -8  -9 -10
desc(factor(letters))
#>  [1]  -1  -2  -3  -4  -5  -6  -7  -8  -9 -10 -11 -12 -13 -14 -15 -16
#> [17] -17 -18 -19 -20 -21 -22 -23 -24 -25 -26

first_day <- seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "years")
desc(first_day)
#>  [1] 21915 21550 21185 20819 20454 20089 19724 19358 18993 18628 18263

starwars |> arrange(desc(mass))
#> # A tibble: 87 × 14
#>    name   height  mass hair_color skin_color eye_color birth_year sex  
#>    <chr>   <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
#>  1 Jabba…    175  1358 NA         green-tan… orange         600   herm…
#>  2 Griev…    216   159 none       brown, wh… green, y…       NA   male 
#>  3 IG-88     200   140 none       metal      red             15   none 
#>  4 Darth…    202   136 none       white      yellow          41.9 male 
#>  5 Tarff…    234   136 brown      brown      blue            NA   male 
#>  6 Owen …    178   120 brown, gr… light      blue            52   male 
#>  7 Bossk     190   113 none       green      red             53   male 
#>  8 Chewb…    228   112 brown      unknown    blue           200   male 
#>  9 Jek T…    180   110 brown      fair       blue            NA   NA   
#> 10 Dexte…    198   102 none       brown      yellow          NA   male 
#> # ℹ 77 more rows
#> # ℹ 6 more variables: gender <chr>, homeworld <chr>, species <chr>,
#> #   films <list>, vehicles <list>, starships <list>
```
