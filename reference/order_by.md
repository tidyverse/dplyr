# A helper function for ordering window function output

This function makes it possible to control the ordering of window
functions in R that don't have a specific ordering parameter. When
translated to SQL it will modify the order clause of the OVER function.

## Usage

``` r
order_by(order_by, call)
```

## Arguments

- order_by:

  a vector to order_by

- call:

  a function call to a window function, where the first argument is the
  vector being operated on

## Details

This function works by changing the `call` to instead call
[`with_order()`](https://dplyr.tidyverse.org/reference/with_order.md)
with the appropriate arguments.

## Examples

``` r
order_by(10:1, cumsum(1:10))
#>  [1] 55 54 52 49 45 40 34 27 19 10
x <- 10:1
y <- 1:10
order_by(x, cumsum(y))
#>  [1] 55 54 52 49 45 40 34 27 19 10

df <- data.frame(year = 2000:2005, value = (0:5) ^ 2)
scrambled <- df[sample(nrow(df)), ]

wrong <- mutate(scrambled, running = cumsum(value))
arrange(wrong, year)
#>   year value running
#> 1 2000     0       4
#> 2 2001     1      39
#> 3 2002     4       4
#> 4 2003     9      38
#> 5 2004    16      55
#> 6 2005    25      29

right <- mutate(scrambled, running = order_by(year, cumsum(value)))
arrange(right, year)
#>   year value running
#> 1 2000     0       0
#> 2 2001     1       1
#> 3 2002     4       5
#> 4 2003     9      14
#> 5 2004    16      30
#> 6 2005    25      55
```
