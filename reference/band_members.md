# Band membership

These data sets describe band members of the Beatles and Rolling Stones.
They are toy data sets that can be displayed in their entirety on a
slide (e.g. to demonstrate a join).

## Usage

``` r
band_members

band_instruments

band_instruments2
```

## Format

Each is a tibble with two variables and three observations

## Details

`band_instruments` and `band_instruments2` contain the same data but use
different column names for the first column of the data set.
`band_instruments` uses `name`, which matches the name of the key column
of `band_members`; `band_instruments2` uses `artist`, which does not.

## Examples

``` r
band_members
#> # A tibble: 3 × 2
#>   name  band   
#>   <chr> <chr>  
#> 1 Mick  Stones 
#> 2 John  Beatles
#> 3 Paul  Beatles
band_instruments
#> # A tibble: 3 × 2
#>   name  plays 
#>   <chr> <chr> 
#> 1 John  guitar
#> 2 Paul  bass  
#> 3 Keith guitar
band_instruments2
#> # A tibble: 3 × 2
#>   artist plays 
#>   <chr>  <chr> 
#> 1 John   guitar
#> 2 Paul   bass  
#> 3 Keith  guitar
```
