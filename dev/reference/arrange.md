# Order rows using column values

`arrange()` orders the rows of a data frame by the values of selected
columns.

Unlike other dplyr verbs, `arrange()` largely ignores grouping; you need
to explicitly mention grouping variables (or use `.by_group = TRUE`) in
order to group by them, and functions of variables are evaluated once
per data frame, not once per group.

## Usage

``` r
arrange(.data, ..., .by_group = FALSE)

# S3 method for class 'data.frame'
arrange(.data, ..., .by_group = FALSE, .locale = NULL)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md) to sort
  a variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

- .locale:

  The locale to sort character vectors in.

  - If `NULL`, the default, uses the `"C"` locale unless the
    `dplyr.legacy_locale` global option escape hatch is active. See the
    [dplyr-locale](https://dplyr.tidyverse.org/dev/reference/dplyr-locale.md)
    help page for more details.

  - If a single string from
    [`stringi::stri_locale_list()`](https://rdrr.io/pkg/stringi/man/stri_locale_list.html)
    is supplied, then this will be used as the locale to sort with. For
    example, `"en"` will sort with the American English locale. This
    requires the stringi package.

  - If `"C"` is supplied, then character vectors will always be sorted
    in the C locale. This does not require stringi and is often much
    faster than supplying a locale identifier.

  The C locale is not the same as English locales, such as `"en"`,
  particularly when it comes to data containing a mix of upper and lower
  case letters. This is explained in more detail on the
  [locale](https://dplyr.tidyverse.org/dev/reference/dplyr-locale.md)
  help page under the `Default locale` section.

## Value

An object of the same type as `.data`. The output has the following
properties:

- All rows appear in the output, but (usually) in a different place.

- Columns are not modified.

- Groups are not modified.

- Data frame attributes are preserved.

## Details

### Missing values

Unlike base sorting with [`sort()`](https://rdrr.io/r/base/sort.html),
`NA` are:

- always sorted to the end for local data, even when wrapped with
  [`desc()`](https://dplyr.tidyverse.org/dev/reference/desc.md).

- treated differently for remote data, depending on the backend.

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dplyr
(`data.frame`) .

## See also

Other single table verbs:
[`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md),
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md),
[`reframe()`](https://dplyr.tidyverse.org/dev/reference/reframe.md),
[`rename()`](https://dplyr.tidyverse.org/dev/reference/rename.md),
[`select()`](https://dplyr.tidyverse.org/dev/reference/select.md),
[`slice()`](https://dplyr.tidyverse.org/dev/reference/slice.md),
[`summarise()`](https://dplyr.tidyverse.org/dev/reference/summarise.md)

## Examples

``` r
arrange(mtcars, cyl, disp)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#>                     carb
#> Toyota Corolla         1
#> Honda Civic            2
#> Fiat 128               1
#> Fiat X1-9              1
#> Lotus Europa           2
#> Datsun 710             1
#> Toyota Corona          1
#> Porsche 914-2          2
#> Volvo 142E             2
#> Merc 230               2
#> Merc 240D              2
#> Ferrari Dino           6
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Merc 280               4
#> Merc 280C              4
#> Valiant                1
#> Hornet 4 Drive         1
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 450SLC            3
#> Maserati Bora          8
#> AMC Javelin            2
#> Dodge Challenger       2
#> Camaro Z28             4
#> Ford Pantera L         4
#> Hornet Sportabout      2
#> Duster 360             4
#> Pontiac Firebird       2
#> Chrysler Imperial      4
#> Lincoln Continental    4
#> Cadillac Fleetwood     4
arrange(mtcars, desc(disp))
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#>                     carb
#> Cadillac Fleetwood     4
#> Lincoln Continental    4
#> Chrysler Imperial      4
#> Pontiac Firebird       2
#> Hornet Sportabout      2
#> Duster 360             4
#> Ford Pantera L         4
#> Camaro Z28             4
#> Dodge Challenger       2
#> AMC Javelin            2
#> Maserati Bora          8
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 450SLC            3
#> Hornet 4 Drive         1
#> Valiant                1
#> Merc 280               4
#> Merc 280C              4
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Merc 240D              2
#> Ferrari Dino           6
#> Merc 230               2
#> Volvo 142E             2
#> Porsche 914-2          2
#> Toyota Corona          1
#> Datsun 710             1
#> Lotus Europa           2
#> Fiat X1-9              1
#> Fiat 128               1
#> Honda Civic            2
#> Toyota Corolla         1

# grouped arrange ignores groups
by_cyl <- mtcars |> group_by(cyl)
by_cyl |> arrange(desc(wt))
#> # A tibble: 32 × 11
#> # Groups:   cyl [3]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#>  2  14.7     8  440    230  3.23  5.34  17.4     0     0     3     4
#>  3  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
#>  4  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#>  5  19.2     8  400    175  3.08  3.84  17.0     0     0     3     2
#>  6  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#>  7  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#>  8  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#>  9  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> 10  15       8  301    335  3.54  3.57  14.6     0     1     5     8
#> # ℹ 22 more rows
# Unless you specifically ask:
by_cyl |> arrange(desc(wt), .by_group = TRUE)
#> # A tibble: 32 × 11
#> # Groups:   cyl [3]
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  24.4     4 147.     62  3.69  3.19  20       1     0     4     2
#>  2  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#>  3  21.4     4 121     109  4.11  2.78  18.6     1     1     4     2
#>  4  21.5     4 120.     97  3.7   2.46  20.0     1     0     3     1
#>  5  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#>  6  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1
#>  7  26       4 120.     91  4.43  2.14  16.7     0     1     5     2
#>  8  27.3     4  79      66  4.08  1.94  18.9     1     1     4     1
#>  9  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1
#> 10  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> # ℹ 22 more rows

# use embracing when wrapping in a function;
# see ?rlang::args_data_masking for more details
tidy_eval_arrange <- function(.data, var) {
  .data |>
    arrange({{ var }})
}
tidy_eval_arrange(mtcars, mpg)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#>                     carb
#> Cadillac Fleetwood     4
#> Lincoln Continental    4
#> Camaro Z28             4
#> Duster 360             4
#> Chrysler Imperial      4
#> Maserati Bora          8
#> Merc 450SLC            3
#> AMC Javelin            2
#> Dodge Challenger       2
#> Ford Pantera L         4
#> Merc 450SE             3
#> Merc 450SL             3
#> Merc 280C              4
#> Valiant                1
#> Hornet Sportabout      2
#> Merc 280               4
#> Pontiac Firebird       2
#> Ferrari Dino           6
#> Mazda RX4              4
#> Mazda RX4 Wag          4
#> Hornet 4 Drive         1
#> Volvo 142E             2
#> Toyota Corona          1
#> Datsun 710             1
#> Merc 230               2
#> Merc 240D              2
#> Porsche 914-2          2
#> Fiat X1-9              1
#> Honda Civic            2
#> Lotus Europa           2
#> Fiat 128               1
#> Toyota Corolla         1

# Use `across()` or `pick()` to select columns with tidy-select
iris |> arrange(pick(starts_with("Sepal")))
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            4.3         3.0          1.1         0.1     setosa
#> 2            4.4         2.9          1.4         0.2     setosa
#> 3            4.4         3.0          1.3         0.2     setosa
#> 4            4.4         3.2          1.3         0.2     setosa
#> 5            4.5         2.3          1.3         0.3     setosa
#> 6            4.6         3.1          1.5         0.2     setosa
#> 7            4.6         3.2          1.4         0.2     setosa
#> 8            4.6         3.4          1.4         0.3     setosa
#> 9            4.6         3.6          1.0         0.2     setosa
#> 10           4.7         3.2          1.3         0.2     setosa
#> 11           4.7         3.2          1.6         0.2     setosa
#> 12           4.8         3.0          1.4         0.1     setosa
#> 13           4.8         3.0          1.4         0.3     setosa
#> 14           4.8         3.1          1.6         0.2     setosa
#> 15           4.8         3.4          1.6         0.2     setosa
#> 16           4.8         3.4          1.9         0.2     setosa
#> 17           4.9         2.4          3.3         1.0 versicolor
#> 18           4.9         2.5          4.5         1.7  virginica
#> 19           4.9         3.0          1.4         0.2     setosa
#> 20           4.9         3.1          1.5         0.1     setosa
#> 21           4.9         3.1          1.5         0.2     setosa
#> 22           4.9         3.6          1.4         0.1     setosa
#> 23           5.0         2.0          3.5         1.0 versicolor
#> 24           5.0         2.3          3.3         1.0 versicolor
#> 25           5.0         3.0          1.6         0.2     setosa
#> 26           5.0         3.2          1.2         0.2     setosa
#> 27           5.0         3.3          1.4         0.2     setosa
#> 28           5.0         3.4          1.5         0.2     setosa
#> 29           5.0         3.4          1.6         0.4     setosa
#> 30           5.0         3.5          1.3         0.3     setosa
#> 31           5.0         3.5          1.6         0.6     setosa
#> 32           5.0         3.6          1.4         0.2     setosa
#> 33           5.1         2.5          3.0         1.1 versicolor
#> 34           5.1         3.3          1.7         0.5     setosa
#> 35           5.1         3.4          1.5         0.2     setosa
#> 36           5.1         3.5          1.4         0.2     setosa
#> 37           5.1         3.5          1.4         0.3     setosa
#> 38           5.1         3.7          1.5         0.4     setosa
#> 39           5.1         3.8          1.5         0.3     setosa
#> 40           5.1         3.8          1.9         0.4     setosa
#> 41           5.1         3.8          1.6         0.2     setosa
#> 42           5.2         2.7          3.9         1.4 versicolor
#> 43           5.2         3.4          1.4         0.2     setosa
#> 44           5.2         3.5          1.5         0.2     setosa
#> 45           5.2         4.1          1.5         0.1     setosa
#> 46           5.3         3.7          1.5         0.2     setosa
#> 47           5.4         3.0          4.5         1.5 versicolor
#> 48           5.4         3.4          1.7         0.2     setosa
#> 49           5.4         3.4          1.5         0.4     setosa
#> 50           5.4         3.7          1.5         0.2     setosa
#> 51           5.4         3.9          1.7         0.4     setosa
#> 52           5.4         3.9          1.3         0.4     setosa
#> 53           5.5         2.3          4.0         1.3 versicolor
#> 54           5.5         2.4          3.8         1.1 versicolor
#> 55           5.5         2.4          3.7         1.0 versicolor
#> 56           5.5         2.5          4.0         1.3 versicolor
#> 57           5.5         2.6          4.4         1.2 versicolor
#> 58           5.5         3.5          1.3         0.2     setosa
#> 59           5.5         4.2          1.4         0.2     setosa
#> 60           5.6         2.5          3.9         1.1 versicolor
#> 61           5.6         2.7          4.2         1.3 versicolor
#> 62           5.6         2.8          4.9         2.0  virginica
#> 63           5.6         2.9          3.6         1.3 versicolor
#> 64           5.6         3.0          4.5         1.5 versicolor
#> 65           5.6         3.0          4.1         1.3 versicolor
#> 66           5.7         2.5          5.0         2.0  virginica
#> 67           5.7         2.6          3.5         1.0 versicolor
#> 68           5.7         2.8          4.5         1.3 versicolor
#> 69           5.7         2.8          4.1         1.3 versicolor
#> 70           5.7         2.9          4.2         1.3 versicolor
#> 71           5.7         3.0          4.2         1.2 versicolor
#> 72           5.7         3.8          1.7         0.3     setosa
#> 73           5.7         4.4          1.5         0.4     setosa
#> 74           5.8         2.6          4.0         1.2 versicolor
#> 75           5.8         2.7          4.1         1.0 versicolor
#> 76           5.8         2.7          3.9         1.2 versicolor
#> 77           5.8         2.7          5.1         1.9  virginica
#> 78           5.8         2.7          5.1         1.9  virginica
#> 79           5.8         2.8          5.1         2.4  virginica
#> 80           5.8         4.0          1.2         0.2     setosa
#> 81           5.9         3.0          4.2         1.5 versicolor
#> 82           5.9         3.0          5.1         1.8  virginica
#> 83           5.9         3.2          4.8         1.8 versicolor
#> 84           6.0         2.2          4.0         1.0 versicolor
#> 85           6.0         2.2          5.0         1.5  virginica
#> 86           6.0         2.7          5.1         1.6 versicolor
#> 87           6.0         2.9          4.5         1.5 versicolor
#> 88           6.0         3.0          4.8         1.8  virginica
#> 89           6.0         3.4          4.5         1.6 versicolor
#> 90           6.1         2.6          5.6         1.4  virginica
#> 91           6.1         2.8          4.0         1.3 versicolor
#> 92           6.1         2.8          4.7         1.2 versicolor
#> 93           6.1         2.9          4.7         1.4 versicolor
#> 94           6.1         3.0          4.6         1.4 versicolor
#> 95           6.1         3.0          4.9         1.8  virginica
#> 96           6.2         2.2          4.5         1.5 versicolor
#> 97           6.2         2.8          4.8         1.8  virginica
#> 98           6.2         2.9          4.3         1.3 versicolor
#> 99           6.2         3.4          5.4         2.3  virginica
#> 100          6.3         2.3          4.4         1.3 versicolor
#> 101          6.3         2.5          4.9         1.5 versicolor
#> 102          6.3         2.5          5.0         1.9  virginica
#> 103          6.3         2.7          4.9         1.8  virginica
#> 104          6.3         2.8          5.1         1.5  virginica
#> 105          6.3         2.9          5.6         1.8  virginica
#> 106          6.3         3.3          4.7         1.6 versicolor
#> 107          6.3         3.3          6.0         2.5  virginica
#> 108          6.3         3.4          5.6         2.4  virginica
#> 109          6.4         2.7          5.3         1.9  virginica
#> 110          6.4         2.8          5.6         2.1  virginica
#> 111          6.4         2.8          5.6         2.2  virginica
#> 112          6.4         2.9          4.3         1.3 versicolor
#> 113          6.4         3.1          5.5         1.8  virginica
#> 114          6.4         3.2          4.5         1.5 versicolor
#> 115          6.4         3.2          5.3         2.3  virginica
#> 116          6.5         2.8          4.6         1.5 versicolor
#> 117          6.5         3.0          5.8         2.2  virginica
#> 118          6.5         3.0          5.5         1.8  virginica
#> 119          6.5         3.0          5.2         2.0  virginica
#> 120          6.5         3.2          5.1         2.0  virginica
#> 121          6.6         2.9          4.6         1.3 versicolor
#> 122          6.6         3.0          4.4         1.4 versicolor
#> 123          6.7         2.5          5.8         1.8  virginica
#> 124          6.7         3.0          5.0         1.7 versicolor
#> 125          6.7         3.0          5.2         2.3  virginica
#> 126          6.7         3.1          4.4         1.4 versicolor
#> 127          6.7         3.1          4.7         1.5 versicolor
#> 128          6.7         3.1          5.6         2.4  virginica
#> 129          6.7         3.3          5.7         2.1  virginica
#> 130          6.7         3.3          5.7         2.5  virginica
#> 131          6.8         2.8          4.8         1.4 versicolor
#> 132          6.8         3.0          5.5         2.1  virginica
#> 133          6.8         3.2          5.9         2.3  virginica
#> 134          6.9         3.1          4.9         1.5 versicolor
#> 135          6.9         3.1          5.4         2.1  virginica
#> 136          6.9         3.1          5.1         2.3  virginica
#> 137          6.9         3.2          5.7         2.3  virginica
#> 138          7.0         3.2          4.7         1.4 versicolor
#> 139          7.1         3.0          5.9         2.1  virginica
#> 140          7.2         3.0          5.8         1.6  virginica
#> 141          7.2         3.2          6.0         1.8  virginica
#> 142          7.2         3.6          6.1         2.5  virginica
#> 143          7.3         2.9          6.3         1.8  virginica
#> 144          7.4         2.8          6.1         1.9  virginica
#> 145          7.6         3.0          6.6         2.1  virginica
#> 146          7.7         2.6          6.9         2.3  virginica
#> 147          7.7         2.8          6.7         2.0  virginica
#> 148          7.7         3.0          6.1         2.3  virginica
#> 149          7.7         3.8          6.7         2.2  virginica
#> 150          7.9         3.8          6.4         2.0  virginica
iris |> arrange(across(starts_with("Sepal"), desc))
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1            7.9         3.8          6.4         2.0  virginica
#> 2            7.7         3.8          6.7         2.2  virginica
#> 3            7.7         3.0          6.1         2.3  virginica
#> 4            7.7         2.8          6.7         2.0  virginica
#> 5            7.7         2.6          6.9         2.3  virginica
#> 6            7.6         3.0          6.6         2.1  virginica
#> 7            7.4         2.8          6.1         1.9  virginica
#> 8            7.3         2.9          6.3         1.8  virginica
#> 9            7.2         3.6          6.1         2.5  virginica
#> 10           7.2         3.2          6.0         1.8  virginica
#> 11           7.2         3.0          5.8         1.6  virginica
#> 12           7.1         3.0          5.9         2.1  virginica
#> 13           7.0         3.2          4.7         1.4 versicolor
#> 14           6.9         3.2          5.7         2.3  virginica
#> 15           6.9         3.1          4.9         1.5 versicolor
#> 16           6.9         3.1          5.4         2.1  virginica
#> 17           6.9         3.1          5.1         2.3  virginica
#> 18           6.8         3.2          5.9         2.3  virginica
#> 19           6.8         3.0          5.5         2.1  virginica
#> 20           6.8         2.8          4.8         1.4 versicolor
#> 21           6.7         3.3          5.7         2.1  virginica
#> 22           6.7         3.3          5.7         2.5  virginica
#> 23           6.7         3.1          4.4         1.4 versicolor
#> 24           6.7         3.1          4.7         1.5 versicolor
#> 25           6.7         3.1          5.6         2.4  virginica
#> 26           6.7         3.0          5.0         1.7 versicolor
#> 27           6.7         3.0          5.2         2.3  virginica
#> 28           6.7         2.5          5.8         1.8  virginica
#> 29           6.6         3.0          4.4         1.4 versicolor
#> 30           6.6         2.9          4.6         1.3 versicolor
#> 31           6.5         3.2          5.1         2.0  virginica
#> 32           6.5         3.0          5.8         2.2  virginica
#> 33           6.5         3.0          5.5         1.8  virginica
#> 34           6.5         3.0          5.2         2.0  virginica
#> 35           6.5         2.8          4.6         1.5 versicolor
#> 36           6.4         3.2          4.5         1.5 versicolor
#> 37           6.4         3.2          5.3         2.3  virginica
#> 38           6.4         3.1          5.5         1.8  virginica
#> 39           6.4         2.9          4.3         1.3 versicolor
#> 40           6.4         2.8          5.6         2.1  virginica
#> 41           6.4         2.8          5.6         2.2  virginica
#> 42           6.4         2.7          5.3         1.9  virginica
#> 43           6.3         3.4          5.6         2.4  virginica
#> 44           6.3         3.3          4.7         1.6 versicolor
#> 45           6.3         3.3          6.0         2.5  virginica
#> 46           6.3         2.9          5.6         1.8  virginica
#> 47           6.3         2.8          5.1         1.5  virginica
#> 48           6.3         2.7          4.9         1.8  virginica
#> 49           6.3         2.5          4.9         1.5 versicolor
#> 50           6.3         2.5          5.0         1.9  virginica
#> 51           6.3         2.3          4.4         1.3 versicolor
#> 52           6.2         3.4          5.4         2.3  virginica
#> 53           6.2         2.9          4.3         1.3 versicolor
#> 54           6.2         2.8          4.8         1.8  virginica
#> 55           6.2         2.2          4.5         1.5 versicolor
#> 56           6.1         3.0          4.6         1.4 versicolor
#> 57           6.1         3.0          4.9         1.8  virginica
#> 58           6.1         2.9          4.7         1.4 versicolor
#> 59           6.1         2.8          4.0         1.3 versicolor
#> 60           6.1         2.8          4.7         1.2 versicolor
#> 61           6.1         2.6          5.6         1.4  virginica
#> 62           6.0         3.4          4.5         1.6 versicolor
#> 63           6.0         3.0          4.8         1.8  virginica
#> 64           6.0         2.9          4.5         1.5 versicolor
#> 65           6.0         2.7          5.1         1.6 versicolor
#> 66           6.0         2.2          4.0         1.0 versicolor
#> 67           6.0         2.2          5.0         1.5  virginica
#> 68           5.9         3.2          4.8         1.8 versicolor
#> 69           5.9         3.0          4.2         1.5 versicolor
#> 70           5.9         3.0          5.1         1.8  virginica
#> 71           5.8         4.0          1.2         0.2     setosa
#> 72           5.8         2.8          5.1         2.4  virginica
#> 73           5.8         2.7          4.1         1.0 versicolor
#> 74           5.8         2.7          3.9         1.2 versicolor
#> 75           5.8         2.7          5.1         1.9  virginica
#> 76           5.8         2.7          5.1         1.9  virginica
#> 77           5.8         2.6          4.0         1.2 versicolor
#> 78           5.7         4.4          1.5         0.4     setosa
#> 79           5.7         3.8          1.7         0.3     setosa
#> 80           5.7         3.0          4.2         1.2 versicolor
#> 81           5.7         2.9          4.2         1.3 versicolor
#> 82           5.7         2.8          4.5         1.3 versicolor
#> 83           5.7         2.8          4.1         1.3 versicolor
#> 84           5.7         2.6          3.5         1.0 versicolor
#> 85           5.7         2.5          5.0         2.0  virginica
#> 86           5.6         3.0          4.5         1.5 versicolor
#> 87           5.6         3.0          4.1         1.3 versicolor
#> 88           5.6         2.9          3.6         1.3 versicolor
#> 89           5.6         2.8          4.9         2.0  virginica
#> 90           5.6         2.7          4.2         1.3 versicolor
#> 91           5.6         2.5          3.9         1.1 versicolor
#> 92           5.5         4.2          1.4         0.2     setosa
#> 93           5.5         3.5          1.3         0.2     setosa
#> 94           5.5         2.6          4.4         1.2 versicolor
#> 95           5.5         2.5          4.0         1.3 versicolor
#> 96           5.5         2.4          3.8         1.1 versicolor
#> 97           5.5         2.4          3.7         1.0 versicolor
#> 98           5.5         2.3          4.0         1.3 versicolor
#> 99           5.4         3.9          1.7         0.4     setosa
#> 100          5.4         3.9          1.3         0.4     setosa
#> 101          5.4         3.7          1.5         0.2     setosa
#> 102          5.4         3.4          1.7         0.2     setosa
#> 103          5.4         3.4          1.5         0.4     setosa
#> 104          5.4         3.0          4.5         1.5 versicolor
#> 105          5.3         3.7          1.5         0.2     setosa
#> 106          5.2         4.1          1.5         0.1     setosa
#> 107          5.2         3.5          1.5         0.2     setosa
#> 108          5.2         3.4          1.4         0.2     setosa
#> 109          5.2         2.7          3.9         1.4 versicolor
#> 110          5.1         3.8          1.5         0.3     setosa
#> 111          5.1         3.8          1.9         0.4     setosa
#> 112          5.1         3.8          1.6         0.2     setosa
#> 113          5.1         3.7          1.5         0.4     setosa
#> 114          5.1         3.5          1.4         0.2     setosa
#> 115          5.1         3.5          1.4         0.3     setosa
#> 116          5.1         3.4          1.5         0.2     setosa
#> 117          5.1         3.3          1.7         0.5     setosa
#> 118          5.1         2.5          3.0         1.1 versicolor
#> 119          5.0         3.6          1.4         0.2     setosa
#> 120          5.0         3.5          1.3         0.3     setosa
#> 121          5.0         3.5          1.6         0.6     setosa
#> 122          5.0         3.4          1.5         0.2     setosa
#> 123          5.0         3.4          1.6         0.4     setosa
#> 124          5.0         3.3          1.4         0.2     setosa
#> 125          5.0         3.2          1.2         0.2     setosa
#> 126          5.0         3.0          1.6         0.2     setosa
#> 127          5.0         2.3          3.3         1.0 versicolor
#> 128          5.0         2.0          3.5         1.0 versicolor
#> 129          4.9         3.6          1.4         0.1     setosa
#> 130          4.9         3.1          1.5         0.1     setosa
#> 131          4.9         3.1          1.5         0.2     setosa
#> 132          4.9         3.0          1.4         0.2     setosa
#> 133          4.9         2.5          4.5         1.7  virginica
#> 134          4.9         2.4          3.3         1.0 versicolor
#> 135          4.8         3.4          1.6         0.2     setosa
#> 136          4.8         3.4          1.9         0.2     setosa
#> 137          4.8         3.1          1.6         0.2     setosa
#> 138          4.8         3.0          1.4         0.1     setosa
#> 139          4.8         3.0          1.4         0.3     setosa
#> 140          4.7         3.2          1.3         0.2     setosa
#> 141          4.7         3.2          1.6         0.2     setosa
#> 142          4.6         3.6          1.0         0.2     setosa
#> 143          4.6         3.4          1.4         0.3     setosa
#> 144          4.6         3.2          1.4         0.2     setosa
#> 145          4.6         3.1          1.5         0.2     setosa
#> 146          4.5         2.3          1.3         0.3     setosa
#> 147          4.4         3.2          1.3         0.2     setosa
#> 148          4.4         3.0          1.3         0.2     setosa
#> 149          4.4         2.9          1.4         0.2     setosa
#> 150          4.3         3.0          1.1         0.1     setosa
```
